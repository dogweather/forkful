---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:12.363150-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u09A6\
  \u09C7\u0996\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2\u0995\u09C7 \u09B2\
  \u09BE\u0987\u09A8 \u0985\u09A8\u09C1\u09AF\u09BE\u09AF\u09BC\u09C0 \u098F\u09AC\
  \u0982 \u09A4\u09BE\u09B0\u09AA\u09B0 \u098F\u0995\u09AC\u09BE\u09B0\u09C7 \u09B8\
  \u09AC \u09AA\u09A1\u09BC\u09BE \u09AF\u09BE\u09AF\u09BC\u0964."
lastmod: '2024-03-17T18:47:44.197564-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u09A6\u09C7\u0996\u09BF \u0995\u09BF\u09AD\u09BE\
  \u09AC\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2\u0995\u09C7 \u09B2\u09BE\u0987\u09A8 \u0985\u09A8\u09C1\u09AF\
  \u09BE\u09AF\u09BC\u09C0 \u098F\u09AC\u0982 \u09A4\u09BE\u09B0\u09AA\u09B0 \u098F\
  \u0995\u09AC\u09BE\u09B0\u09C7 \u09B8\u09AC \u09AA\u09A1\u09BC\u09BE \u09AF\u09BE\
  \u09AF\u09BC\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
চলুন দেখি কিভাবে একটি টেক্সট ফাইলকে লাইন অনুযায়ী এবং তারপর একবারে সব পড়া যায়।

```Lua
-- ফাইল পড়ুন লাইন অনুযায়ী
local file = io.open("example.txt", "r") -- ফাইলটি পড়ার জন্য ওপেন করুন
if file then
  for line in file:lines() do -- ফাইলের প্রতিটি লাইনের উপর ইটারেট করা
    print(line)
  end
  file:close() -- আপনি সমাপ্ত হলে সর্বদা ফাইলটি বন্ধ করুন
else
  print("ফাইল খোলা যাচ্ছে না।")
end

-- একবারে পুরো ফাইল পড়ুন
local file = io.open("example.txt", "r") -- ফাইলটি পড়ার জন্য ওপেন করুন
if file then
  local content = file:read("*a") -- পুরো বিষয়বস্তু পড়ুন
  print(content)
  file:close() -- ফাইলটি বন্ধ করুন
else
  print("ফাইল খোলা যাচ্ছে না।")
end
```

যদি `example.txt` ফাইলে থাকে:
```
Hello, Lua!
```

তাহলে আউটপুট হবে:
```
Hello, Lua!
```

## গভীরে যাওয়া
ঐতিহাসিকভাবে, ফাইল পড়া একটি মৌলিক অপারেশন, যা প্রাথমিক কম্পিউটারগুলি থেকে চলে আসছে। Lua তে, এটি `io` লাইব্রেরি দ্বারা সহজ I/O মডেলের মাধ্যমে পরিচালিত হয়।

একটি ফাইলের বিষয়বস্তু অ্যাক্সেস করার জন্য `io.lines` এবং `io.read` সাধারণ উপায়, তবে প্রগতিশীল ফাইল অপারেশনের জন্য `lfs` (LuaFileSystem) এর মতো বিকল্প উপায় রয়েছে।

পড়ার সময়, Lua পর্দার পেছনে বাফারিং পরিচালনা করে, তবে বড় ফাইলের ক্ষেত্রে, উচ্চ মেমোরি ব্যবহার এড়াতে চাঙ্কগুলিতে পড়া উচিত।

`io` লাইব্রেরি ব্যবহার সরল, তবে সবসময় সিদ্ধান্ত গ্রহণে ফাইল বন্ধ করা উচিত যাতে সম্পদ লিক না হয়। ত্রুটির ক্ষেত্রে, Lua ফাইল অপারেশনগুলি `nil` এবং একটি ত্রুটি বার্তা ফেরত দেয়, যা আপনাকে সঠিকতার জন্য সম্ভালনা করা উচিত।

## আরও দেখুন:
- [Lua 5.4 রেফারেন্স ম্যানুয়াল: I/O](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Lua শিখুন](https://learnxinyminutes.com/docs/lua/)
