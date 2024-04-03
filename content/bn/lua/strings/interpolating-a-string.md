---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:47.632095-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua-\u09A4\u09C7, \u0995\u09A8\
  \u0995\u09CD\u09AF\u09BE\u099F\u09C7\u09A8\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF `..` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\
  \ \u0985\u09A5\u09AC\u09BE \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\
  \u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `string.format` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3."
lastmod: '2024-03-17T18:47:44.163342-06:00'
model: gpt-4-0125-preview
summary: "Lua-\u09A4\u09C7, \u0995\u09A8\u0995\u09CD\u09AF\u09BE\u099F\u09C7\u09A8\
  \u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `..` \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8 \u0985\u09A5\u09AC\u09BE \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF `string.format` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C1\u09A8\u0964 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
Lua-তে, কনক্যাটেনেশনের জন্য `..` ব্যবহার করুন অথবা ইন্টারপোলেশনের জন্য `string.format` ব্যবহার করুন। উদাহরণ:
```Lua
local name = "Ada"
local greeting = "Hello, " .. name .. "!"
print(greeting) -- আউটপুট: Hello, Ada!

local age = 30
local bio = string.format("%s is %d years old.", name, age)
print(bio) -- আউটপুট: Ada is 30 years old.
```

## গভীর ডুব
ঐতিহাসিক দিক থেকে, Lua-তে কিছু অন্যান্য ভাষার মতো (উদাঃ, Ruby, Python) বিল্ট-ইন স্ট্রিং ইন্টারপোলেশন ছিল না। `..` দিয়ে কনক্যাটেনেশন ছিল প্রধান পদ্ধতি। Lua 5.3 `string.format` পরিচিত করে এনেছে, সি-এর `printf`-এর মতো একটি পরিষ্কার প্রোচেসের জন্য। **বিকল্পগুলি:** `..` অপারেটর অথবা `string.format` ব্যবহার করা ছাড়াও, আপনি একটি কাস্টম ইন্টারপোলেশন ফাংশন লিখতে পারেন যা gsub ব্যবহার করে প্যাটার্ন মিলানোর জন্য। কিন্তু কেন জিনিসগুলোকে জটিল করবেন? রক্ষণাবেক্ষণের জন্য বিল্ট-ইন টুলগুলি ব্যবহার করুন। **বাস্তবায়নের বিস্তারিত:** বুঝতে হবে যে ঘন ঘন স্ট্রিং কনক্যাটেনেশন পারফরমেন্স সমস্যা তৈরি করতে পারে। `string.format` তখন উপকারী যখন আপনার নিয়ন্ত্রণের প্রয়োজন, যেমন নম্বর প্রিসিশন বা প্যাডিং নির্দিষ্ট করা।

## দেখুন আরো
- স্ট্রিংস সম্পর্কিত Lua ম্যানুয়াল: http://www.lua.org/manual/5.4/manual.html#6.4
- 'প্রোগ্রামিং ইন Lua' স্ট্রিংস উপর: https://www.lua.org/pil/20.1.html
- Lua-ইউজার্স উইকি স্ট্রিংস উপর: http://lua-users.org/wiki/StringLibraryTutorial
