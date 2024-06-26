---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:26.512612-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u098F\u09B0 JavaScript\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB JSON \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\
  \u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u09A8\
  \u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 **Aeson** \u098F\u09B0 \u09AE\
  \u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09DF-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF\u09C7 JSON \u09A8\u09BF\u09DF\u09A8\u09CD\u09A4\u09CD\u09B0\
  \u09A3 \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u09B9\u09DF\u09C7\u2026"
lastmod: '2024-04-05T21:53:52.509354-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F\u09B0 JavaScript \u098F\u09B0 \u09AE\u09A4\u09CB JSON \u098F\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\
  \u09AE\u09B0\u09CD\u09A5\u09A8 \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\
  \u09C1 **Aeson** \u098F\u09B0 \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09DF\
  -\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF\u09B0 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 JSON \u09A8\u09BF\
  \u09DF\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3 \u0995\u09B0\u09BE \u09B8\u09B9\u099C\
  \ \u09B9\u09DF\u09C7 \u09AF\u09BE\u09DF\u0964 Aeson \u0989\u099A\u09CD\u099A-\u09B8\
  \u09CD\u09A4\u09B0 \u098F\u09AC\u0982 \u09A8\u09BF\u09AE\u09CD\u09A8-\u09B8\u09CD\
  \u09A4\u09B0\u09C7\u09B0 \u09AB\u09BE\u0982\u09B6\u09A8 \u0989\u09AD\u09AF\u09BC\
  \u0987 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7 \u098F\u09A8\u0995\
  \u09CB\u09A1\u09BF\u0982 (Haskell \u09AE\u09BE\u09A8\u0997\u09C1\u09B2\u09BF \u0995\
  \u09C7 JSON \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0) \u098F\u09AC\
  \u0982 \u09A1\u09BF\u0995\u09CB\u09A1\u09BF\u0982 (JSON \u0995\u09C7 Haskell \u09AE\
  \u09BE\u09A8\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE) \u098F\u09B0\
  \ \u099C\u09A8\u09CD\u09AF\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

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
