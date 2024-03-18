---
changelog:
- 2024-01-30, dogweather, reviewed and added links
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:52.435903-06:00
description: "REPL \u098F\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3\u09B0\u09C2\u09AA \u09B9\
  \u09B2 Read-Eval-Print Loop, \u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09C7\u0995\u09CD\u099F\u09BF\u09AD \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\
  \ \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u0995\u09CB\u09A1 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u098F\u099F\u09BF \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE, \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BF\u0982, \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.180885-06:00'
model: gpt-4-0125-preview
summary: "REPL \u098F\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3\u09B0\u09C2\u09AA \u09B9\
  \u09B2 Read-Eval-Print Loop, \u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09C7\u0995\u09CD\u099F\u09BF\u09AD \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\
  \ \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u0995\u09CB\u09A1 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u098F\u099F\u09BF \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09BE, \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BF\u0982, \u098F\u09AC\u0982\u2026"
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?
REPL এর পূর্ণরূপ হল Read-Eval-Print Loop, একটি ইন্টারেক্টিভ পরিবেশ যেখানে আপনি দ্রুত কোড পরীক্ষা করতে পারেন। প্রোগ্রামারগণ এটি পরীক্ষা করা, ডিবাগিং, এবং একটি ভাষার বৈশিষ্ট্যগুলি শিখতে ব্যবহার করে থাকেন।

## কিভাবে:
Lua-র REPL-এ প্রবেশ করতে, কেবল `lua` টাইপ করুন আপনার টার্মিনালে। এখানে একটি সেশনের উদাহরণ দেওয়া হলঃ

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
সেশনে, আমরা একটি ভেরিয়েবল ঘোষণা করি, মৌলিক অংক করি, একটি টেবিল পরিচালনা করি, এবং এর আইটেমগুলির মাধ্যমে লুপ করি।

## গভীর ডুব
Lua-র হালকা প্রকৃতি এর REPL-কে প্রোটোটাইপিংয়ের জন্য আদর্শ করে তোলে। এটি ১৯৯০-এর দশকের প্রারম্ভে Lua-র সৃষ্টি থেকে বিদ্যমান, Lisp এর মতো ভাষাগুলির পূর্ববর্তী ইন্টারেক্টিভ শেলগুলি দ্বারা অনুপ্রাণিত। অন্যান্য ভাষার বিকল্পগুলো অন্তর্ভুক্ত করে `irb` রাবির জন্য এবং `python` পাইথনের জন্য, প্রতিটির ভিন্ন ফিচার সেট সহ। Lua-র REPL মিনিমালিস্টিক; অর্থাৎ, এটি অন্যান্যদের মধ্যে পাওয়া জটিল ডিবাগিং টুলগুলির মতো উন্নত ফিচার অভাব করতে পারে। একটি আরও সম্পূর্ণ অভিজ্ঞতার জন্য, ZeroBrane Studio বা LuaDist-এর LuaRocks-এর মতো টুলগুলি প্রাথমিক REPL এর উপরে বেশি অফার করে।

## দেখুন সেইসাথে
- [Lua 5.4 রেফারেন্স ম্যানুয়াল - স্ট্যান্ডঅ্যালোন Lua ইন্টারপ্রেটার](https://www.lua.org/manual/5.4/manual.html#7)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
