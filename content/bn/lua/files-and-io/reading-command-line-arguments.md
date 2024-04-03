---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:19.139646-06:00
description: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\
  \u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8 \u09AA\u09A1\u09BC\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\
  \u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u09A8\u09BE\u09AE\u09C7\u09B0\
  \ \u09AA\u09B0\u09C7 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u099F\u09BE\u0987\u09AA\
  \ \u0995\u09B0\u09BE \u0985\u09A4\u09BF\u09B0\u09BF\u0995\u09CD\u09A4 \u099F\u09C1\
  \u0995\u09B0\u09CB\u0997\u09C1\u09B2\u09BF \u0986\u09AF\u09BC\u09A4\u09CD\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0995\u09CB\u09A1 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\
  \u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.195354-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8 \u09AA\u09A1\u09BC\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\
  \u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7\u09B0 \u09A8\u09BE\u09AE\u09C7\u09B0\
  \ \u09AA\u09B0\u09C7 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u099F\u09BE\u0987\u09AA\
  \ \u0995\u09B0\u09BE \u0985\u09A4\u09BF\u09B0\u09BF\u0995\u09CD\u09A4 \u099F\u09C1\
  \u0995\u09B0\u09CB\u0997\u09C1\u09B2\u09BF \u0986\u09AF\u09BC\u09A4\u09CD\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0995\u09CB\u09A1 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\
  \u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u0986\u099A\u09B0\u09A3\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09A4\u09C7 \u098F\
  \u099F\u09BF \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\u09A8 \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0987\u09B2 \u0996\u09CB\u09B2\u09BE \u09AC\u09BE \u0986\u0989\u099F\
  \u09AA\u09C1\u099F\u09C7\u09B0 \u09AC\u09BE\u0995\u09CD\u09AF\u09BE\u09B2\u09BE\u09AA\
  \u09C7\u09B0 \u09AE\u09BE\u09A4\u09CD\u09B0\u09BE \u09A8\u09BF\u09B0\u09CD\u09A7\
  \u09BE\u09B0\u09A3 \u0995\u09B0\u09BE\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
এখানে Lua-তে সেই আর্গুমেন্টগুলি আয়ত্তের উপর সংক্ষিপ্ত বিবরণ:

```Lua
-- এটি 'greet.lua' হিসাবে সংরক্ষণ করুন
local name = arg[1] -- arg[1] হল প্রথম কমান্ড লাইন আর্গুমেন্ট
print("Hello, " .. (name or "অপরিচিত") .. "!")
```

টার্মিনাল চালু করুন এবং এটি চালান:

```
$ lua greet.lua LuaLearner
Hello, LuaLearner!
```

নাম নেই? সমস্যা নেই:

```
$ lua greet.lua
Hello, অপরিচিত!
```

## গভীর ডাইভ
Lua গ্লোবাল `arg` টেবিলের সাথে সহজ ব্যাপার রাখে। ঐতিহাসিকভাবে, লোকেরা প্রোগ্রামিংয়ে কমান্ড লাইন আর্গুমেন্টগুলি পড়ার কাজ করে আসছে সময়ের সূচনালগ্ন থেকে (অন্তত, UNIX এর জন্ম থেকে নিশ্চিত)। এটি কাস্টমাইজেশনের একটি মূল উপাদান।

Lua-তে, `arg` হল একটি অ্যারে যাতে কমান্ড-লাইনের সমস্ত চমকপ্রদ উপাদান থাকে। `arg[0]` হল স্ক্রিপ্টের নাম, এবং `arg[1]` থেকে শুরু করে বাকি গুলো হল আসল আর্গুমেন্টস। যদি আপনি আরও বেশি আনন্দ চান, তবে একটি লুপের মাধ্যমে সবগুলো উত্তোলন করতে পারেন:

```Lua
for i = 1, #arg do
  print("Argument " .. i .. ": " .. arg[i])
end
```

বিকল্পগুলি? নিশ্চিত, জটিল আর্গুমেন্ট পার্সিংয়ের জন্য বাহ্যিক লাইব্রেরিগুলি রয়েছে (যেমন `Penlight`), কিন্তু অনেক ক্ষেত্রে, `arg` বিনা ঝামেলায় কাজ করে।

বাস্তবায়নের বিবরণের কথা বলতে গেলে, Lua-র অ্যারেগুলি 1-ভিত্তিক (তারা 1-এ গণনা শুরু করে), অনেক অন্যান্য ভাষার মতো 0 নয়। এই কারণেই `arg[1]` হল প্রথম আর্গুমেন্ট এবং `arg[0]` নয়।

## আরও দেখুন
আরও বেশি জানতে চাওয়ার জন্য, এখানে কিছু অতিরিক্ত খাদ্য রয়েছে:

- `arg` টেবিলের উপর Lua 5.4 রেফারেন্স ম্যানুয়াল: https://www.lua.org/manual/5.4/manual.html#6.1
- Lua-র মৌলিক ধারণা পেতে "Programming in Lua" (চতুর্থ সংস্করণ): https://www.lua.org/pil/contents.html
- Penlight, উন্নত আর্গুমেন্ট পার্সিং সহ একটি Lua ইউটিলিটি লাইব্রেরি: https://github.com/lunarmodules/Penlight
