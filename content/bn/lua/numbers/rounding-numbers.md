---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:08.205796-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u0997\u09CB\u09B2\u0995\
  \u09B0\u09A3 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0\u0995\u09C7 \u09A8\u09BF\u0995\u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\
  \u09A3\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0985\u09A5\u09AC\u09BE \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A6\u09B6\u09AE\u09BF\u0995 \u09B8\
  \u09CD\u09A5\u09BE\u09A8\u09C7 \u09B8\u09AE\u09A8\u09CD\u09AC\u09AF\u09BC \u0995\
  \u09B0\u09BE\u0964 \u098F\u099F\u09BF \u099C\u099F\u09BF\u09B2\u09A4\u09BE \u09B9\
  \u09CD\u09B0\u09BE\u09B8 \u0995\u09B0\u09BE, \u0995\u09B0\u09CD\u09AE\u0995\u09CD\
  \u09B7\u09AE\u09A4\u09BE \u09AC\u09C3\u09A6\u09CD\u09A7\u09BF \u0995\u09B0\u09BE\
  , \u098F\u09AC\u0982 \u09AF\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.173801-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u0997\u09CB\u09B2\u0995\
  \u09B0\u09A3 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\
  \u09B0\u0995\u09C7 \u09A8\u09BF\u0995\u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\
  \u09A3\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0985\u09A5\u09AC\u09BE \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A6\u09B6\u09AE\u09BF\u0995 \u09B8\
  \u09CD\u09A5\u09BE\u09A8\u09C7 \u09B8\u09AE\u09A8\u09CD\u09AC\u09AF\u09BC \u0995\
  \u09B0\u09BE\u0964 \u098F\u099F\u09BF \u099C\u099F\u09BF\u09B2\u09A4\u09BE \u09B9\
  \u09CD\u09B0\u09BE\u09B8 \u0995\u09B0\u09BE, \u0995\u09B0\u09CD\u09AE\u0995\u09CD\
  \u09B7\u09AE\u09A4\u09BE \u09AC\u09C3\u09A6\u09CD\u09A7\u09BF \u0995\u09B0\u09BE\
  , \u098F\u09AC\u0982 \u09AF\u09C7\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কি এবং কেন?
সংখ্যাকে গোলকরণ করা মানে তাদেরকে নিকটতম পূর্ণসংখ্যা অথবা নির্দিষ্ট দশমিক স্থানে সমন্বয় করা। এটি জটিলতা হ্রাস করা, কর্মক্ষমতা বৃদ্ধি করা, এবং যে সময়গুলোতে নির্দিষ্ট একটি বিন্দুর বেশি সঠিকতা মূল্য যোগ না করে তেমন সময়গুলোতে প্রোগ্রামিংয়ের একটি মৌলিক দিক।

## কিভাবে:
```lua
-- Lua তে বেসিক গোলকরণ অন্তর্ভুক্ত নেই, তবে আপনি একটি ফাংশন নির্ধারণ করতে পারেন:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- নির্দিষ্ট একটি দশমিক স্থানে গোলকরণ করার জন্য:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## গভীর ডুব
Lua অন্যান্য কিছু ভাষার মত বক্সের বাইরে গোলকরণ ফাংশন অন্তর্ভুক্ত করে না। ঐতিহাসিকভাবে, আপনাকে নিজের ফাংশন লিখতে হবে অথবা তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করতে হবে। প্রচলিত উপায়গুলি নীচে নামার জন্য `math.floor()` এবং উপরে উঠার জন্য `math.ceil()` ব্যবহার করে, এবং তা করার আগে সংখ্যার চিহ্ন অনুযায়ী 0.5 যোগ অথবা বিয়োগ করে।

নিজের ফাংশন তৈরির বিকল্প অন্তর্ভুক্ত "lua-users wiki" অথবা "Penlight" মত লাইব্রেরিগুলি। প্রতিটির নিজস্ব সুবিধা এবং বাধা আছে, যেমন অতিরিক্ত বৈশিষ্ট্য বা অধিক ওভারহেড।

অভ্যন্তরীণভাবে, এই ফাংশনগুলি সাধারণত কম্পিউটারগুলি কীভাবে ফ্লোটিং-পয়েন্ট সংখ্যাগুলি সঞ্চয় করে তা ব্যবহার করে কাজ করে। আপনি যে পজিটিভ ফ্লোটটি গোলকরণ করতে চান তাতে 0.5 যোগ করলে, তা পরবর্তী পূর্ণসংখ্যা মানের সীমানা অতিক্রম করে, তাই যখন আপনি `math.floor()` প্রয়োগ করেন তা নিকটতম পূর্ণসংখ্যাতে নামায়।

## আরও দেখুন
- [Lua 5.4 রেফারেন্স ম্যানুয়াল: গাণিতিক ফাংশনস](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua লাইব্রেরি: ম্যাথ](https://github.com/lunarmodules/Penlight)
