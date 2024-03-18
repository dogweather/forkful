---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:26.512612-06:00
description: "Haskell \u098F JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09B9\u09B2\u09CB JSON \u09A1\u09C7\u099F\u09BE \u0995\u09C7 Haskell \u099F\u09BE\
  \u0987\u09AA\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\
  \u0982 Haskell \u099F\u09BE\u0987\u09AA \u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u0986\
  \u09AC\u09BE\u09B0 JSON \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.106835-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2\u09CB JSON \u09A1\u09C7\u099F\u09BE \u0995\u09C7 Haskell \u099F\u09BE\u0987\
  \u09AA\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982\
  \ Haskell \u099F\u09BE\u0987\u09AA \u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u0986\u09AC\
  \u09BE\u09B0 JSON \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
Haskell এ JSON (JavaScript Object Notation) এর সাথে কাজ করা মানে হলো JSON ডেটা কে Haskell টাইপে পার্স করা এবং Haskell টাইপ গুলোকে আবার JSON এ রূপান্তর করা। প্রোগ্রামারগণ এটি করেন যাতে তাদের Haskell অ্যাপ্লিকেশানগুলি সহজেই ওয়েব সার্ভিস বা API সমূহের সাথে ডেটা আদান-প্রদান করতে পারে, যা আধুনিক সফটওয়্যার ডেভেলপমেন্টে ক্রস-প্ল্যাটফর্ম ডেটা ইন্টারচেঞ্জের জন্য একটি সাধারণ অনুশীলন।

## কিভাবে:
Haskell এর JavaScript এর মতো JSON এর জন্য নির্মিত সমর্থন নেই, কিন্তু **Aeson** এর মতো তৃতীয়-পক্ষের লাইব্রেরির সাহায্যে JSON নিয়ন্ত্রণ করা সহজ হয়ে যায়। Aeson উচ্চ-স্তর এবং নিম্ন-স্তরের ফাংশন উভয়ই প্রদান করে এনকোডিং (Haskell মানগুলি কে JSON এ রূপান্তর) এবং ডিকোডিং (JSON কে Haskell মানে পার্স করা) এর জন্য।

### Aeson ইনস্টল করা
প্রথমে, আপনার প্রজেক্টের নির্ভরতা আপডেট করে Aeson যোগ করে নিন আপনার `.cabal` ফাইল আপডেট করে বা সরাসরি Stack বা Cabal ব্যবহার করে:

```shell
cabal update && cabal install aeson
```
অথবা, আপনি যদি Stack ব্যবহার করেন:
```shell
stack install aeson
```

### JSON পার্স করা
আসুন Haskell টাইপে JSON ডেটা ডিকোড করার একটি বেসিক উদাহরণ দেখি। ধরা যাক আমাদের কাছে নিম্নলিখিত JSON রয়েছে যা একজন ব্যক্তির প্রতিনিধিত্ব করে:

```json
{
  "name": "John Doe",
  "age": 30
}
```

প্রথমে, একটি সম্পর্কিত Haskell ডেটা টাইপ নির্ধারণ করুন এবং তাকে `FromJSON` এর একটি ইন্সট্যান্স করুন:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- ফাইল থেকে JSON ডিকোড করার ফাংশন
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
ব্যবহার:
ধরা যাক, `person.json` উপরে দেখানো JSON ডেটা ধারণ করে, চালান:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
নমুনা আউটপুট:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Haskell মানগুলি কে JSON হিসেবে এনকোড করা
কোনও Haskell মানকে আবার JSON এ রূপান্তর করতে, আপনার `ToJSON` এর একটি ইন্সট্যান্স হিসাবে আপনার টাইপটি নির্ধারণ করতে হবে এবং তারপর `encode` ব্যবহার করতে হবে।

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- ধরে নেওয়া হচ্ছে আগের Person টাইপ 

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
নমুনা আউটপুট:
```json
{"name":"Jane Doe","age":32}
```

এই উদাহরণগুলি Aeson ব্যবহার করে Haskell এ JSON নিয়ে কাজ করার মৌলিকত্ব দেখায়। মনে রাখবেন, Aeson অনেক বেশি কিছু অফার করে, যা বিভিন্ন প্রয়োজন এবং পরিস্থিতিতে উপযোগী, যেমন কাস্টম পার্সিং নিয়ম, জটিল নেস্টেড JSON নিয়ে কাজ করা, এবং আরও অনেক কিছু।
