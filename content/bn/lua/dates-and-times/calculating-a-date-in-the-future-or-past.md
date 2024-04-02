---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:37.981046-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\
  \u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09AA\
  \u09B0\u09C7 \u09AC\u09BE \u0986\u0997\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u099F\
  \u09BF \u0995\u09BF \u09B9\u09AC\u09C7 \u09A4\u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09B0\u09BF\u09AE\u09BE\u0987\u09A8\u09CD\u09A1\u09BE\
  \u09B0, \u09B8\u09BE\u09AC\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09B6\u09A8\
  \ \u09AC\u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u0998\u099F\u09A8\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.193315-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE \u0985\
  \u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\u09B8\
  \u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A8\u09BF\u09B0\u09CD\
  \u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09AA\u09B0\
  \u09C7 \u09AC\u09BE \u0986\u0997\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u099F\u09BF\
  \ \u0995\u09BF \u09B9\u09AC\u09C7 \u09A4\u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u09B0\u09BF\u09AE\u09BE\u0987\u09A8\u09CD\u09A1\u09BE\u09B0\
  , \u09B8\u09BE\u09AC\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09B6\u09A8 \u09AC\
  \u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u0998\u099F\u09A8\u09BE\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি এবং কেন?
ভবিষ্যত অথবা অতীতের তারিখ হিসেব করা মানে নির্দিষ্ট সময়ের পরে বা আগে তারিখটি কি হবে তা বের করা। প্রোগ্রামাররা এটি রিমাইন্ডার, সাবস্ক্রিপশন বা অতীতের ঘটনা ট্র্যাক করার মতো বৈশিষ্ট্যগুলির জন্য করে থাকেন।

## কিভাবে:

Lua-এ, আপনি তারিখ এবং সময় হিসাবে `os.date` এবং `os.time` ফাংশন ব্যবহার করে সাহায্য নিতে পারেন।

```Lua
-- বর্তমান তারিখে দিন যোগ
local daysToAdd = 10
local futureDate = os.time() + (daysToAdd * 24 * 60 * 60) -- দিন * ঘন্টা * মিনিট * সেকেন্ড
print("ভবিষ্যত তারিখ: " .. os.date("%Y-%m-%d", futureDate))

-- বর্তমান তারিখ থেকে দিন বিয়োগ
local daysToSubtract = 5
local pastDate = os.time() - (daysToSubtract * 24 * 60 * 60) -- উপরের মতো রূপান্তর
print("অতীত তারিখ: " .. os.date("%Y-%m-%d", pastDate))
```

নমুনা আউটপুট হতে পারে:
```
ভবিষ্যত তারিখ: 2023-05-03
অতীত তারিখ: 2023-04-18
```

## গভীর ডুব

Lua-এর `os.date` এবং `os.time` ফাংশনগুলি স্ট্যান্ডার্ড C লাইব্রেরিতে তাদের মূল পায়। এর মানে হল তারা মেশিনের অত্যন্ত নিকটে — দক্ষ এবং নির্ভরযোগ্য। তারা সময় অঞ্চল বা দিনের আলো সাশ্রয়ী সময়ের মতো জটিল জিনিসের সাথে মোকাবিলা না করে ইউটিসি এবং ইউনিক্স এপক (জানুয়ারি 1, 1970) থেকে সেকেন্ডের হিসাবে লেনদেন করে।

`os.date` এবং `os.time` এর বিকল্প রয়েছে যদি আপনি আরও সন্ধান করেন। `Luadate` এর মতো লাইব্রেরিগুলি সময় অঞ্চল এবং দিনের আলো সাশ্রয়ী সময় আরও সূচকভাবে সামাল দেয়।

বাস্তবায়নের ক্ষেত্রে, লিপ সেকেন্ড নজরে রাখুন, এবং মনে রাখবেন যে 30 দিন যোগ করা মাস যোগ করার মতো সহজ নয়। বিভিন্ন মাসে বিভিন্ন দিনের সংখ্যা রয়েছে, এবং ফেব্রুয়ারি আপনাকে হয় প্রতারনা করতে পারে অথবা একটি অতিরিক্ত দিনের সাথে চমক দিতে পারে।

## আরও দেখুন

Lua-এ আরও ভাল তারিখ এবং সময় অভিজ্ঞতার জন্য, এই সংস্থানগুলি দেখুন:

- LuaRocks `Luadate`: https://luarocks.org/modules/luarocks/luadate
- Lua-ইউজার্স উইকি তারিখ এবং সময়ের উপর: http://lua-users.org/wiki/DateTime
- Lua 5.4 ম্যানুয়ালে `os` লাইব্রেরি রেফারেন্স: https://www.lua.org/manual/5.4/manual.html#6.9
