---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:59.856828-06:00
description: "\u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u09A1\u09BF\u09AC\u09BE\u0997\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09A8\u09C7\
  \ \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\
  \u09B0\u09BE \u09AF\u09BE\u09A4\u09C7 \u0986\u09AA\u09A8\u09BF \u09AC\u09C1\u099D\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\
  \u09CB\u09A1\u09C7 \u0995\u09BF \u0998\u099F\u099B\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7\u09A8 \u09AE\u09C7\u09B6\u09BF\u09A8\u09C7\u09B0 \u09AE\u09A7\
  \u09CD\u09AF\u09C7 \u09A5\u09BE\u0995\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.181903-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u09A1\u09BF\u09AC\u09BE\u0997\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09A8\u09C7\
  \ \u09A4\u09A5\u09CD\u09AF \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\
  \u09B0\u09BE \u09AF\u09BE\u09A4\u09C7 \u0986\u09AA\u09A8\u09BF \u09AC\u09C1\u099D\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\
  \u09CB\u09A1\u09C7 \u0995\u09BF \u0998\u099F\u099B\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7\u09A8 \u09AE\u09C7\u09B6\u09BF\u09A8\u09C7\u09B0 \u09AE\u09A7\
  \u09CD\u09AF\u09C7 \u09A5\u09BE\u0995\u09BE\u2026"
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কি এবং কেন?
প্রিন্ট ডিবাগ আউটপুট মানে হলো আপনার স্ক্রিনে তথ্য প্রদর্শন করা যাতে আপনি বুঝতে পারেন আপনার কোডে কি ঘটছে। প্রোগ্রামাররা এটি করেন মেশিনের মধ্যে থাকা গ্রেম্লিনস—বাগসমূহ খুঁজে বের করার জন্য।

## কিভাবে:
এখানে লুয়াতে জিনিস প্রিন্ট করার উপায় উপস্থাপন করা হল:

```Lua
print("Hello, Debug World!")  -- কনসোলে একটি স্ট্রিং প্রদর্শন করে

local number = 42
print("The number is:", number)  -- স্ট্রিং এবং সংখ্যা একসাথে মিলান

local table = {name = "Lua", year = 1993}
print(table)  -- টেবিল রেফারেন্স প্রিন্ট করে, বিশেষ সাহায্যকারী নয়
```

নমুনা আউটপুট:
```
Hello, Debug World!
The number is: 42
table: 0x194a330
```

টেবিলের ভেতরে গিয়ে তার গোপনীয়তা দেখানোর জন্য এটি করুন:

```Lua
for key, value in pairs(table) do
    print(key, "=", value)
end
```

নমুনা আউটপুট:
```
name = Lua
year = 1993
```

## গভীর ডুব
প্রিন্ট ডিবাগ আউটপুট নতুন কিছু নয় বা আড়ম্বরপূর্ণও নয়। এটি একটি পুরানো হাতুড়ির মতো বিশ্বস্ত। দেখুন, অতীতে, আড়ম্বরপূর্ণ ডিবাগারগুলি চালু ছিল না। কোডাররা কোথায় গন্ডগোল হচ্ছে তা দেখার জন্য প্রিন্ট করতো। লুয়ার `print` ফাংশন সরল। এটি আপনার টার্মিনালে সাধারণত জিনিসপ্রতি স্ট্যান্ডার্ড আউটপুটে প্রেরণ করে।

বিকল্প পদ্ধতিরা? লুয়াতে গুচ্ছ আছে। যেমন আপনি যদি আরও কন্ট্রোল চান, যেমন নতুন লাইন এড়িয়ে যাওয়া তাহলে আরও ভারী `io.write()` আছে। `inspect` মতো মডিউলগুলি প্রিন্টের চেয়ে আপনার টেবিলগুলির গভীর তথ্য উন্মোচন করতে পারে।

লুয়ার C সোর্স কোডে `print` এর বাস্তবায়ন মৌলিক। এটি প্রতিটি আর্গুমেন্টে `tostring` ব্যবহার করে এবং একটি নতুন লাইনের সাথে `stdout`-এ পাঠায়। LuaJIT, লুয়ার একটি জাস্ট-ইন-টাইম কম্পাইলার সংস্করণ, একই `print` পদ্ধতি ব্যবহার করে, কিন্তু আরও নিরাপদ।

## আরও দেখুন
বৃহত্তর পরিসরের জ্ঞান লাভ করুন:

- লুয়ার অফিসিয়াল `print` ডকুমেন্টেশন: https://www.lua.org/manual/5.4/manual.html#pdf-print
- LuaJIT সম্পর্কে একটি ভূমিকা: http://luajit.org/intro.html
- `io` লাইব্রেরির বর্ণনা, `io.write` সম্পর্কে অধিক তথ্যের জন্য: https://www.lua.org/manual/5.4/manual.html#6.8
- `inspect.lua` মডিউল, যখন আপনি আপনার টেবিলগুলিকে লজ্জা পাওয়ার কারণে ক্লান্ত: https://github.com/kikito/inspect.lua
