---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.675793-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u098F \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\u09A8\u09BE\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AE\
  \u09B0\u09BE `os.time()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\
  \u09B2\u09BF\u0995\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE\u09B8\u09C2\u099A\u0995 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\
  \ (epoch time) \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.192336-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u098F \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\
  \u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AE\u09B0\u09BE `os.time()` \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u098F\u0995\
  \u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B8\u09C2\u099A\u0995 \u09AB\
  \u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 (epoch time) \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09A4\u09C7 \u098F\u09AC\u0982 \u09A4\
  \u09BE\u09B0\u09AA\u09B0 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\
  \u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09BF\u0964 \u09B8\u09B9\u099C\
  \ \u09AC\u09BF\u09B7\u09AF\u09BC\u0964."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
Lua-এ বিল্ট-ইন তারিখ তুলনা ফাংশন নেই, তবে আমরা `os.time()` ফাংশন ব্যবহার করে তারিখগুলিকে একটি সংখ্যাসূচক ফর্ম্যাটে (epoch time) রূপান্তর করতে এবং তারপর তাদের তুলনা করতে পারি। সহজ বিষয়।

```Lua
-- তারিখগুলিকে epoch time-এ রূপান্তর করুন
local date1 = os.time({year=2023, month=4, day=1})
local date2 = os.time({year=2023, month=4, day=15})

-- তারিখগুলি তুলনা করুন
if date1 > date2 then
  print("Date1 হল Date2 এর চেয়ে পরে।")
elseif date1 < date2 then
  print("Date1 হল Date2 এর চেয়ে আগে।")
else
  print("Date1 এবং Date2 একই।")
end
```

এই তারিখগুলি দিয়ে চালালে মোটামুটি আউটপুট আসবে:

```
Date1 হল Date2 এর চেয়ে আগে।
```

## গভীর ডুব
আগের দিনে, Lua-এ তারিখ টাইপ দিয়ে আসেনি। প্রোগ্রামাররা তারিখ-সময় অপারেশনগুলির জন্য `os.time()` ফাংশনের উপর নির্ভর করেছিলেন, যা আজও ব্যবহৃত হয়। `os.time()` ফাংশন যুগের (অর্থাৎ ইউনিক্স সময়, যা জানুয়ারী 1, 1970 তারিখে শুরু) থেকে সেকেন্ডে সময় ফিরিয়ে আনে। এটি কার্যকর কারণ এটি তারিখগুলিকে সংখ্যায় রূপান্তর করে, তুলনা সহজ করে তোলে।

বিকল্প হিসেবে, আপনি তারিখ টেবিলের জন্য একটি কাস্টম তুলনাকারী লিখতে পারেন, প্রতিটি ক্ষেত্র (বছর, মাস, দিন) ম্যানুয়ালি তুলনা করতে পারেন, বা `LuaDate` এর মতো তৃতীয়-পক্ষের তারিখ লাইব্রেরি ব্যবহার করতে পারেন।

`os.time()` ব্যবহার করার সময়, সময় অঞ্চল এবং দিবালোক সঞ্চয় পরিবর্তনের বিষয়ে সচেতন থাকুন। অন্যথা নির্দিষ্ট না করা পর্যন্ত ফাংশনটি আপনি স্থানীয় সময় প্রদান করছেন বলে ধরে নেয়।

## দেখুন
- Lua 5.4 রেফারেন্স ম্যানুয়াল: https://www.lua.org/manual/5.4/
- LuaDate, একটি তারিখ এবং সময় মডিউল: https://github.com/Tieske/date
- ইউনিক্স টাইমস্ট্যাম্প বোঝার জন্য: https://en.wikipedia.org/wiki/Unix_time
