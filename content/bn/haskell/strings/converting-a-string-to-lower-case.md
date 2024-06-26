---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:44.235391-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Haskell \u0985\u0995\u09CD\u09B7\
  \u09B0\u0997\u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\
  \u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `Data.Char` \u09AE\u09A1\u09BF\u0989\
  \u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964 `toLower`\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09BF\u09B6\u09C7\u09B7\u09AD\u09BE\u09AC\
  \u09C7 \u098F\u0995\u0995 \u0985\u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u09B2\u09CB\
  \u09AF\u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4\
  \ \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.068317-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09A8\u09BF\
  \u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ `Data.Char` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7\u0964 `toLower` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09BF\
  \u09B6\u09C7\u09B7\u09AD\u09BE\u09AC\u09C7 \u098F\u0995\u0995 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\
  \u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\
  \u09BF \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B8\u09AE\u09CD\u09AA\u09C2\u09B0\u09CD\u09A3\u09B0\u09C2\u09AA\u09C7\
  \ \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\
  \u09A3\u09A4 \u0995\u09B0\u09A4\u09C7 \u098F\u0987 \u09AB\u09BE\u0982\u09B6\u09A8\
  \u099F\u09BF\u09B0 \u0993\u09AA\u09B0 \u09AE\u09CD\u09AF\u09BE\u09AA\u09BF\u0982\
  \ \u0995\u09B0\u09AC\u09C7\u09A8\u0964 \u0995\u09CB\u09A1\u099F\u09BF \u09A6\u09C7\
  \u0996\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
Haskell অক্ষরগুলি নিয়ন্ত্রণের জন্য `Data.Char` মডিউল ব্যবহার করে। `toLower` ফাংশন বিশেষভাবে একক অক্ষরকে লোয়ার কেসে পরিণত করে। আপনি একটি স্ট্রিংকে সম্পূর্ণরূপে লোয়ার কেসে পরিণত করতে এই ফাংশনটির ওপর ম্যাপিং করবেন। কোডটি দেখুন:

```haskell
import Data.Char (toLower)

-- একটি স্ট্রিংকে লোয়ার কেসে পরিণত করুন
lowercaseString :: String -> String
lowercaseString = map toLower

-- ব্যবহার
main :: IO ()
main = putStrLn $ lowercaseString "Hello, Haskell!"
```

নমুনা আউটপুট:

```
hello, haskell!
```

## গভীর ডাইভ
ঐতিহাসিকভাবে, অক্ষরের কেস ধারণাটি ম্যানুয়াল টাইপসেটিং যুগ থেকে এসেছে যখন আপারকেস এবং লোয়ারকেস অক্ষরগুলি আলাদা কেসে সংরক্ষিত হত। প্রোগ্রামিংয়ে, কেস রূপান্তর বিশেষ করে কেস-অনুবেদনহীন অপারেশনগুলিতে একরূপতা নিশ্চিত করে।

Haskell বিশেষত্বের উপরে একটি দৃষ্টিপাত। `Data.Char` মডিউলটি, যেখানে `toLower` অবস্থিত, Haskell 98 মানদণ্ডে আবির্ভাব ঘটে। এটি সেই থেকে অক্ষর নিয়ন্ত্রণের জন্য যাওয়া-আসা করে থাকে। অন্যান্য ভাষাগুলিতে তাদের নিজস্ব পদ্ধতি রয়েছে, যেমন JavaScript-এ `.toLowerCase()` বা Python-এ `.lower()`, কিন্তু Haskell-এ, `map` এবং `toLower` দক্ষতার সাথে কাজ করে।

অভ্যন্তরে, `toLower` Unicode বিবেচনা করে, অর্থাৎ এটি মৌলিক ASCII পরিসীমা ছাড়িয়ে বিপুল পরিমাণে অক্ষর এবং স্ক্রিপ্ট সামাল দিতে পারে - আন্তর্জাতিকীকরণের জন্য সহায়ক।

বিকল্প? নিশ্চিত ভাবে, আপনি `toLower`-এর অনুকরণে নিজের ফাংশন তৈরি করতে পারেন, কিন্তু চাকা আবার আবিষ্কার কেন করবেন? `Data.Char`-এর দিকে থাকুন পঠনযোগ্যতা এবং নির্ভরযোগ্যতার জন্য। পাশাপাশি, `text` এবং `bytestring` মতো লাইব্রেরিগুলি বড় ডেটাসেটের সাথে কাজ করার বা কর্মক্ষমতা লক্ষ্যের জন্য আরও কর্মক্ষমতা-সম্মত পদ্ধতি প্রদান করে।

## আরও দেখুন
- `Data.Char` ডকুমেন্টেশন: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- Haskell 98 রিপোর্ট অন `Data.Char`: https://www.haskell.org/onlinereport/standard-prelude.html
- Haskell এর জন্য Text লাইব্রেরি: https://hackage.haskell.org/package/text
- Haskell এর জন্য ByteString লাইব্রেরি: https://hackage.haskell.org/package/bytestring
