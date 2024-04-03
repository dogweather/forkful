---
changelog:
- 2024-01-30, dogweather, reviewed and added links
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:52.435903-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u09B0 REPL-\u098F \u09AA\u09CD\
  \u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09A4\u09C7, \u0995\u09C7\u09AC\u09B2 `lua`\
  \ \u099F\u09BE\u0987\u09AA \u0995\u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09C7\u09B6\u09A8\u09C7\u09B0\
  \ \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\
  \u09B2\u0983."
lastmod: '2024-03-17T18:47:44.180885-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u09B0 REPL-\u098F \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\
  \u09A4\u09C7, \u0995\u09C7\u09AC\u09B2 `lua` \u099F\u09BE\u0987\u09AA \u0995\u09B0\
  \u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\
  \u09A8\u09BE\u09B2\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09C7\u09B6\u09A8\u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

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
