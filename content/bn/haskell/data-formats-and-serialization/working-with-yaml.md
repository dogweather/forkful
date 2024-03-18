---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:37.428300-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
