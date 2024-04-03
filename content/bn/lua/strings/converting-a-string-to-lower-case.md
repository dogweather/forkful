---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:54.294975-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua \u09A4\u09C7, \u0986\u09AA\
  \u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u09AC\u09CB\u099D\u09BE \u09B9\u09BE\
  \u09B2\u0995\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 `string.lower()`\
  \ \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\u0964 \u098F\u099F\u09BF\u09A4\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09BE\u0993\
  \u09DF\u09BE\u0993, \u09AC\u09C7\u09B0 \u09B9\u09DF\u09C7 \u0986\u09B8\u09C7 \u09A4\
  \u09BE\u09B0 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u09AD\u09BE\u09B0\
  \u09CD\u09B8\u09A8\u0964 \u09B2\u0995\u09CD\u09B7\u09CD\u09AF \u0995\u09B0\u09C1\
  \u09A8."
lastmod: '2024-03-17T18:47:44.164463-06:00'
model: gpt-4-0125-preview
summary: "Lua \u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09AC\u09CB\u099D\u09BE \u09B9\u09BE\u09B2\u0995\u09BE \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8 `string.lower()` \u098F\u09B0 \u09B8\u09BE\u09A5\
  \u09C7\u0964 \u098F\u099F\u09BF\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u0996\u09BE\u0993\u09DF\u09BE\u0993, \u09AC\u09C7\
  \u09B0 \u09B9\u09DF\u09C7 \u0986\u09B8\u09C7 \u09A4\u09BE\u09B0 \u099B\u09CB\u099F\
  \ \u09B9\u09BE\u09A4\u09C7\u09B0 \u09AD\u09BE\u09B0\u09CD\u09B8\u09A8\u0964 \u09B2\
  \u0995\u09CD\u09B7\u09CD\u09AF \u0995\u09B0\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
Lua তে, আপনি আপনার বোঝা হালকা করতে পারেন `string.lower()` এর সাথে। এটিতে একটি স্ট্রিং খাওয়াও, বের হয়ে আসে তার ছোট হাতের ভার্সন। লক্ষ্য করুন:

```lua
local originalString = "Hello, World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- আউটপুট: hello, world!
```

এই স্নিপেটটি চালান। চিত্কার করা বড় হাতের অক্ষরগুলো এখন ফিসফিসিয়ে বলার মত ছোট হাতের অক্ষরে।

## গভীর ডাইভ
কম্পিউটিং শুরু হওয়ার সময় থেকেই, মানুষজন নানা কারণে টেক্সটকে একইভাবে কেসে পরিণত করার প্রয়োজন অনুভব করেছে, যেমন সর্টিং অথবা কেস-ইনসেনসিটিভ লগইনের মত কারণে। Lua তে, `string.lower()` এর আবির্ভাব থেকেই এটি ছিল প্রধান পছন্দ। এটি পরিপাটি, এটি বিল্ট-ইন, এবং এটি তার কাজ করে বিনা ঝামেলায়।

কিন্তু আড়ালে কি আছে? `string.lower()` প্রতিটি অক্ষরের মাধ্যমে দ্রুতগতিতে চলে যায়, এবং যদি এটি বড় হাতের হয় (A থেকে Z), এটি পরিবর্তন করে। Lua আসলে ASCII মানগুলির ওপর নির্ভর করে: 'A' (65) থেকে 'Z' (90) পর্যন্ত হয়ে যায় 'a' (97) থেকে 'z' (122) পর্যন্ত। পার্থক্যটা কি? 32. তাই, `lowercase = uppercase + 32`।

কি হবে যদি `string.lower()` খুব সাধারণ মনে হয়? আপনি ম্যানুয়ালি ASCII মানগুলি বা `string.gsub()` এর সাথে প্যাটার্ন ম্যাচিং ব্যবহার করে অক্ষরগুলির মধ্যে দিয়ে হাঁটতে পারেন:

```lua
local s = "Make Me Lowercase, Please"
s = s:gsub("%u", function (upper) return string.char(upper:byte() + 32) end)
print(s)  -- আউটপুট: make me lowercase, please
```

কিন্তু আসলে, যখন আপনার কাছে একটি আউটবোর্ড মোটর (অর্থাৎ `string.lower()`) আছে, তখন নৌকা বেয়ে কেন?

## আরো দেখুন
Lua-র স্ট্রিং ম্যানিপুলেশন সম্পর্কে এই ভালো জিনিসগুলি দিয়ে আরও গভীরে ডাইভ করুন:
- [Programming in Lua (4th edition)](https://www.lua.org/pil/contents.html) স্ট্রিংয়ের ভিতর, বাইরে এবং মাঝামাঝি সম্পর্কে।
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4) যখন আপনি ছোট হাতের অক্ষরের বাইরে যাওয়ার জন্য প্রস্তুত, তখন সমস্ত স্ট্রিং ফাংশনের জন্য।
