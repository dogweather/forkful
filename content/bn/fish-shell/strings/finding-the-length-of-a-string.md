---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:04.565770-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u09AB\u09BF\u09B6\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u09AA\u09BE\
  \u09AC\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\
  \u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.486462-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 \u09AB\u09BF\u09B6\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u09AA\u09BE\u09AC\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
এখানে ফিশে একটি স্ট্রিং এর দৈর্ঘ্য পাবার উপায় দেওয়া হল:

```Fish Shell
set my_string "Hello, World!"
echo (string length "$my_string")
```

আউটপুট:

```
১৩
```

## গভীর ডুব
Fish এ, অন্যান্য কিছু শেলর তুলনায়, `string length` একটি বিল্ট-ইন ফাংশন যা এটিকে স্বাভাবিক এবং দক্ষ করে তোলে। ঐতিহাসিকভাবে, অন্যান্য শেলগুলি `expr` অথবা `wc` এর মতো বাহ্যিক টুলস বা বেশি বাচাল সিনট্যাক্স প্রয়োজন হতে পারে। Fish, এর শক্তিশালী স্ট্রিং হ্যান্ডলিং ফাংশনগুলির সাথে কাজ সহজ করে, যেখানে `string length` সরাসরি ইউনিকোড ক্যারেক্টারগুলির গণনা দেয়, যা সর্বদা বাইটের সংখ্যার সমান নয়, বিশেষ করে নন-ASCII ক্যারেক্টারগুলির ক্ষেত্রে।

Fish এ 'string' ফাংশনের আগে শেলগুলিতে স্ট্রিং দৈর্ঘ্য নির্ধারণের বিকল্প উপায়গুলি কম নির্ভরযোগ্য হতে পারে কারণ এগুলি প্রায়ই মাল্টিবাইট ক্যারেক্টারগুলিকে অ্যাকাউন্টে নিত না। বাস্তবায়নের দিক থেকে, `string length` ইউনিকোড গ্রাফেমগুলির গণনা করে, যা এমন টেক্সটগুলির জন্য গুরুত্বপূর্ণ যেগুলিতে ক্যারেক্টারগুলি অন্যান্যদের সাথে মিলিত হয়ে একক দৃশ্যমান ইউনিট গঠন করে।

## দেখুন আরও
- Fish ডকুমেন্টেশন স্ট্রিং ম্যানিপুলেশনের উপর: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- গ্রাফেমগুলি বোঝার জন্য ইউনিকোড স্ট্যান্ডার্ড: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
