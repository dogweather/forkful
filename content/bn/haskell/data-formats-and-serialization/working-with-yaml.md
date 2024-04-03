---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:37.428300-06:00
description: "YAML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3\u09B0\u09C2\u09AA\
  \ \"YAML Ain't Markup Language\", \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09A8\u09AC\
  -\u09AC\u09BE\u09A8\u09CD\u09A7\u09AC \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AE\u09BE\u09A8\
  \u09A6\u09A3\u09CD\u09A1 \u09AF\u09BE \u09B8\u0995\u09B2 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AD\u09BE\u09B7\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:44.105862-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3\u09B0\u09C2\u09AA\
  \ \"YAML Ain't Markup Language\", \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09A8\u09AC\
  -\u09AC\u09BE\u09A8\u09CD\u09A7\u09AC \u09A1\u09C7\u099F\u09BE \u09B8\u09BF\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AE\u09BE\u09A8\
  \u09A6\u09A3\u09CD\u09A1 \u09AF\u09BE \u09B8\u0995\u09B2 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AD\u09BE\u09B7\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u098F\
  \u09B0 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE \u098F\u09AC\
  \u0982 \u09B8\u09B0\u09B2 \u0997\u09A0\u09A8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\
  \u09C7 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\
  \u0987\u09B2 \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE\u0997\u09C1\u09B2\u09BF\
  \u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09C7\u099F\u09BE \u09AC\u09BF\u09A8\
  \u09BF\u09AE\u09AF\u09BC\u09C7 YAML \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কি এবং কেন?

YAML, যার পূর্ণরূপ "YAML Ain't Markup Language", একটি মানব-বান্ধব ডেটা সিরিয়ালাইজেশন মানদণ্ড যা সকল প্রোগ্রামিং ভাষার জন্য ব্যবহৃত হতে পারে। প্রোগ্রামাররা প্রায়ই এর পাঠযোগ্যতা এবং সরল গঠনের কারণে কনফিগারেশন ফাইল এবং ভাষাগুলির মধ্যে ডেটা বিনিময়ে YAML ব্যবহার করে থাকেন।

## কিভাবে:

Haskell এ YAML প্রক্রিয়াজাতিতে নির্মিত সমর্থন নেই, তবে আপনি `yaml` এবং `aeson` এর মত তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করে YAML ডেটা পার্সিং এবং জেনারেট করতে পারেন। এখানে আপনি কিভাবে শুরু করতে পারেন:
### YAML পড়া

প্রথমত, আপনার প্রকল্পের নির্ভরতা হিসেবে `yaml` প্যাকেজ যোগ করুন। তারপরে, আপনি নিম্নলিখিত উদাহরণ ব্যবহার করে একটি সাধারণ YAML ডকুমেন্ট পার্স করতে পারেন:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- উদাহরণ YAML ডেটা
yamlData :: ByteString
yamlData = "
name: জন ডো
age: 30
"

-- একটি ডেটা কাঠামো সংজ্ঞা দিন যা YAML ডকুমেন্টের সাথে মিলে যায়
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "YAML পার্সিং ত্রুটি: " ++ show err
    Right person -> print person
```
উপরের কোডের জন্য নমুনা আউটপুট হতে পারে:
```
Person {name = "জন ডো", age = 30}
```

### YAML লেখা
Haskell ডেটা কাঠামো থেকে YAML জেনারেট করার জন্য, নিচের মত করে `yaml` প্যাকেজের এনকোডিং ফাংশনালিটি ব্যবহার করতে পারেন:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- পূর্ববর্তী উদাহরণ থেকে Person ডেটা কাঠামো ব্যবহার করা হচ্ছে

person :: Person
person = Person "জেন ডো" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
এই প্রোগ্রামের আউটপুট হবে একটি YAML-ফরম্যাটেড স্ট্রিং:
```
name: জেন ডো
age: 25
```

এই উদাহরণগুলি Haskell এ YAML নিয়ে কাজ করার শুরুতে সাহায্য করা উচিত। আপনার প্রয়োজনানুসারে, আপনি এই লাইব্রেরিগুলি দ্বারা প্রদত্ত আরো উন্নত বৈশিষ্ট্য এবং বিকল্পগুলি অন্বেষণ করতে চাইতে পারেন।
