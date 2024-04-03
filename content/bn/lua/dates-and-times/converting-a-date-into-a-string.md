---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:21.014746-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u09A4\u09C7, \u0986\u09AE\
  \u09B0\u09BE `os.date` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u09DF\u09C7 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u0995\u09B0\u09BF\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0995\u09CB\u09A1\
  \u09C7\u09B0 \u0985\u0982\u09B6 \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2\u0964\
  ."
lastmod: '2024-03-17T18:47:44.191326-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u09A4\u09C7, \u0986\u09AE\u09B0\u09BE `os.date` \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7 \u09AB\u09B0\u09AE\u09CD\
  \u09AF\u09BE\u099F \u0995\u09B0\u09BF\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u0995\u09CB\u09A1\u09C7\u09B0 \u0985\u0982\u09B6 \u09A6\u09C7\
  \u0993\u09DF\u09BE \u09B9\u09B2\u0964."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
Lua-তে, আমরা `os.date` ব্যবহার করে তারিখকে স্ট্রিংয়ে ফরম্যাট করি। এখানে একটি কোডের অংশ দেওয়া হল।

```lua
local now = os.time()
local formatted = os.date("%Y-%m-%d %H:%M:%S", now)
print(formatted)
-- উদাহরণ আউটপুট: 2023-04-01 15:24:37
```

ভিন্ন ধরনের ফরম্যাট চান? স্ট্রিং প্যাটার্নটি কাস্টমাইজ করুন।

```lua
local friendly_format = os.date("%B %d, %Y")
print(friendly_format)
-- উদাহরণ আউটপুট: April 01, 2023
```

## গভীরে যাওয়া
Lua-এর `os.date` ফাংশনটি POSIX `strftime` ফাংশনের আদলে তৈরি। যদি খুঁটিয়ে দেখা যায়, তবে এটি C-এর `printf` পরিবারের মতো—একই মূলের।

বিকল্প? নিশ্চয়ই। আপনি স্ট্রিং কনক্যাটেনেশন এবং টেবিল ইনডেক্সিং নিয়ে কুস্তি করতে পারেন—ম্যানুয়ালি তারিখের অংশগুলি গ্রহণ করে। কিন্তু যখন `os.date` এটি সহজে সামলাতে পারে, তখন পরিশ্রম কেন করবেন?

বাস্তবায়নের বিস্তারিত? `os.date` ফাংশন দুইভাবে আচরণ করতে পারে:
- একটি ফরম্যাট স্ট্রিং প্রদান করা হলে, এটি ফরম্যাট করা তারিখ ফেরত দেয়।
- ফরম্যাটটি বাদ দিলে, এটি তারিখের উপাদানগুলি সম্বলিত একটি তালিকা ফেরত দেয়।

মজার তথ্য: Lua-র সময়-সংক্রান্ত ফাংশনগুলি যুগের সংখ্যাগুলির সাথে কাজ করে—Jan 1, 1970 থেকে সেকেন্ডের সংখ্যা। এই বৈশিষ্ট্যটি Unix সময়ের থেকে মূলত আসা।

## আরও দেখুন
- `os.date` এর উপর Lua-র রেফারেন্স ম্যানুয়াল: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
- `os.date` কে সুস্বাদু করতে strftime ফরম্যাট স্পেসিফায়ার: http://strftime.org/
- Unix যুগের সময় সম্পর্কে উৎসুকদের জন্য দীপ্ত আলোচনা: https://en.wikipedia.org/wiki/Unix_time
