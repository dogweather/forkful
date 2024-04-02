---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:42.802724-06:00
description: "Haskell \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 TOML \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7\
  \ \u09B9\u09B2\u09CB TOML (Tom's Obvious, Minimal Language) \u09A1\u09BE\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u0989\u09CE\
  \u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE \u09AE\u09C2\
  \u09B2\u09A4 \u09B8\u09B9\u099C\u09C7\u0987 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\
  \u09B0\u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.108798-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 TOML \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\
  \u09B2\u09CB TOML (Tom's Obvious, Minimal Language) \u09A1\u09BE\u099F\u09BE \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u0989\u09CE\u09AA\
  \u09A8\u09CD\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BE \u09AE\u09C2\u09B2\
  \u09A4 \u09B8\u09B9\u099C\u09C7\u0987 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\
  \u09C7\u09B6\u09A8\u2026"
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি ও কেন?
Haskell এর সাথে TOML নিয়ে কাজ করার মানে হলো TOML (Tom's Obvious, Minimal Language) ডাটা পার্স করা এবং উৎপন্ন করা। প্রোগ্রামাররা এটা মূলত সহজেই কনফিগারেশন ফাইল বা ডাটা ইন্টারচেঞ্জ পরিচালনা করতে চান, যেখানে শক্তিশালী টাইপ গ্যারান্টি এবং মিনিমাল সিনট্যাক্স বিঘ্ন থাকে।

## কিভাবে:
প্রথমে, নিশ্চিত করুন আপনি TOML পার্সিং লাইব্রেরি আছে। Haskell এর জন্য, `htoml` একটি জনপ্রিয় বিকল্প। আপনাকে এটি আপনার প্রজেক্টের ডিপেন্ডেন্সিতে যোগ করতে হবে।

```Haskell
-- TOML পার্সিং লাইব্রেরি আমদানি 
import qualified Text.Toml as Toml

-- আপনার কনফিগ ডাটা স্ট্রাকচার ডিফাইন করুন
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- ঐচ্ছিক তারিখ
} deriving (Show)

-- একটি TOML স্ট্রিং পার্স করা
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "ত্রুটি: " ++ show err
    Right toml -> print toml -- অথবা পার্স করা TOML আরও প্রক্রিয়া করুন
```

নমুনা আউটপুট যেকোনো Haskell ডাটা টাইপের মত কাঠামো ও অ্যাক্সেস প্রদান করা যেতে পারে।

## গভীর ডুব
ঐতিহাসিকভাবে, TOML টম প্রেস্টন-ওয়ার্নার, GitHub-এর সহ-প্রতিষ্ঠাতা, দ্বারা কনফিগারেশন ফাইলের জন্য YAML ও JSON-এর জটিলতার প্রতিক্রিয়া হিসেবে তৈরি করা হয়েছিল। এটি JSON এর চেয়ে আরও পঠনীয় এবং লেখার জন্য সহজ হওয়ার জোর দেয়, এবং YAML এর চেয়ে আরও কঠোর ও সহজ।

TOML-এর বিকল্প হলো JSON ও YAML, প্রতিটি ফরম্যাটের নিজস্ব শক্তি রয়েছে। JSON সর্বুদ্ধিমান ও ভাষা-নিরপেক্ষ, অন্যদিকে YAML আরো মানব-পঠনযোগ্য ফর্ম্যাট অফার করে। TOML এর সারল্য ও ধারাবাহিকতা এর মূল্যবান মনে করা হয়, এর আত্মীয়দের কিছু ফাঁদ এড়াতে।

Haskell-এ বাস্তবায়ন সাধারণত এমন একটি লাইব্রেরির মাধ্যমে করা হয় যা TOML কে Haskell ডাটা টাইপে পার্স করে, প্রায়শই Haskell-এর উন্নত টাইপ সিস্টেমের সুবিধা নিয়ে সঠিকতা নিশ্চিত করে। পার্সিং সহজাত অবদান অথবা কম্বিনেটর পার্সিং এর মাধ্যমে করা যেতে পারে, যা কোডের দক্ষতা, পঠনীয়তা ও রক্ষণাবেক্ষণযোগ্যতার মধ্যে সমন্বয় ঘটায়।

## আরো দেখুন
- `htoml`: https://hackage.haskell.org/package/htoml
- আনুষ্ঠানিক TOML GitHub রেপোজিটরি: https://github.com/toml-lang/toml
- ডাটা সিরিয়ালাইজেশন ফরম্যাটের তুলনা: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
