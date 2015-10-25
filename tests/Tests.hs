{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2015 John Millikin <john@john-millikin.com>
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main
	( tests
	, main
	) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IDN.IDNA as IDNA
import qualified Data.Text.IDN.Punycode as PC
import qualified Data.Text.IDN.StringPrep as SP
import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

tests :: [Suite]
tests =
	[ suite_IDNA
	, suite_Punycode
	, suite_StringPrep
	]

suite_IDNA :: Suite
suite_IDNA = suite "idna"
	[ test_IDNA_toASCII
	, test_IDNA_toUnicode
	]

test_IDNA_toASCII :: Test
test_IDNA_toASCII = assertions "toASCII" $ do
	let toASCII = IDNA.toASCII
	let defaultFlags = IDNA.defaultFlags
	$expect $ equal
		(toASCII defaultFlags (t "helloworld.com"))
		(Right (b8 "helloworld.com"))
	$expect $ equal
		(toASCII defaultFlags (t "преве́д.com"))
		(Right (b8 "xn--lsa32dhabb1dh.com"))

	-- verifySTD3 permits non-hostname strings.
	$expect $ left (toASCII defaultFlags (t "not valid"))
	$expect $ equal
		(toASCII defaultFlags{IDNA.verifySTD3=False} (t "not valid"))
		(Right (b8 "not valid"))

	-- allowUnassigned permits unassigned codepoints.
	$expect $ left (toASCII defaultFlags (t "\x070E"))
	$expect $ equal
		(toASCII defaultFlags{IDNA.allowUnassigned=True} (t "\x070E"))
		(Right (b8 "xn--7mb"))

test_IDNA_toUnicode :: Test
test_IDNA_toUnicode = assertions "toUnicode" $ do
	let toUnicode = IDNA.toUnicode
	let defaultFlags = IDNA.defaultFlags
	$expect $ equal
		(toUnicode defaultFlags (b8 "helloworld.com"))
		(t "helloworld.com")
	$expect $ equal
		(toUnicode defaultFlags (b8 "xn--lsa32dhabb1dh.com"))
		(t "преве́д.com")

	-- allowUnassigned permits unassigned codepoints.
	$expect $ equal
		(toUnicode defaultFlags (b8 "xn--7mb"))
		(t "xn--7mb")
	$expect $ equal
		(toUnicode defaultFlags{IDNA.allowUnassigned=True} (b8 "xn--7mb"))
		(t "\x070E")

suite_Punycode :: Suite
suite_Punycode = suite "punycode"
	[ test_Punycode_Encode
	, test_Punycode_Decode
	]

test_Punycode_Encode :: Test
test_Punycode_Encode = assertions "encode" $ do
	$expect (equal
		(PC.encode (t "преве́д") Nothing)
		(b8 "lsa32dhabb1dh"))
	$expect (equal
		(PC.encode (t "преве́д") (Just (== 0)))
		(b8 "lsa32dhabb1Dh"))

test_Punycode_Decode :: Test
test_Punycode_Decode = assertions "decode" $ do
	do
		let dec = PC.decode (b8 "lsa32dhabb1dh")
		$assert (just dec)
		let Just (txt,_) = dec
		$expect (equal txt (t "преве́д"))
	do
		let dec = PC.decode (b8 "lsa32dhabb1Dh")
		$assert (just dec)
		let Just (txt,fn) = dec
		$expect (equal txt (t "преве́д"))
		$expect (equal (fn 0) True)
		$expect (equal (fn 1) False)

suite_StringPrep :: Suite
suite_StringPrep = suite "stringprep"
	[ test_StringPrep_SASL
	]

test_StringPrep_SASL :: Test
test_StringPrep_SASL = assertions "sasl" $ do
	-- http://www.ietf.org/mail-archive/web/sasl/current/msg02723.html
	-- http://www.ietf.org/mail-archive/web/sasl/current/msg02722.html
	let prep s = SP.stringprep SP.sasl SP.defaultFlags (t s)
	$expect $ equal (prep "x\xADy") (Right (t "xy"))
	$expect $ equal (prep "user") (Right (t "user"))
	$expect $ equal (prep "USER") (Right (t "USER"))
	$expect $ equal (prep "\x2163") (Right (t "IV"))
	$expect $ left (prep "\x7")
	$expect $ left (prep "\x627\x31")
	$expect $ equal (prep "\xAA") (Right (t "a"))

main :: IO ()
main = Test.Chell.defaultMain tests

t :: String -> T.Text
t = T.pack

b8 :: String -> B8.ByteString
b8 = B8.pack
