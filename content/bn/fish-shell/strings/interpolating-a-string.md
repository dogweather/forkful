---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:23.617854-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8\
  : Fish \u098F, \u0986\u09AA\u09A8\u09BF \u09A1\u09AC\u09B2 \u0995\u09CB\u099F\u09C7\
  \u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u09A8\
  \ \u098F\u09AC\u0982 \u09AF\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\
  \u09B2 \u09AC\u09BE \u0995\u09AE\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1 \u0986\u09AA\
  \u09A8\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u099F\
  \ \u0995\u09B0\u09A4\u09C7 \u099A\u09BE\u09A8 \u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u098F\u0995\u099F\u09BF \u09A1\u09B2\
  \u09BE\u09B0 \u099A\u09BF\u09B9\u09CD\u09A8 `$` \u09B0\u09BE\u0996\u09C7\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.481669-06:00'
model: gpt-4-0125-preview
summary: "Fish \u098F, \u0986\u09AA\u09A8\u09BF \u09A1\u09AC\u09B2 \u0995\u09CB\u099F\
  \u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \u09A8 \u098F\u09AC\u0982 \u09AF\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\
  \u09AC\u09B2 \u09AC\u09BE \u0995\u09AE\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1 \u0986\
  \u09AA\u09A8\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u099F\
  \ \u0995\u09B0\u09A4\u09C7 \u099A\u09BE\u09A8 \u09A4\u09BE\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u098F\u0995\u099F\u09BF \u09A1\u09B2\
  \u09BE\u09B0 \u099A\u09BF\u09B9\u09CD\u09A8 `$` \u09B0\u09BE\u0996\u09C7\u09A8 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7\
  \u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে করবেন:
Fish এ, আপনি ডবল কোটেশন ব্যবহার করেন এবং যে ভেরিয়েবল বা কম্যান্ড আপনি ইন্টারপোলেট করতে চান তার সাথে সরাসরি একটি ডলার চিহ্ন `$` রাখেন স্ট্রিং এর মধ্যে।

```fish
set name "world"
echo "Hello, $name!"
```

আউটপুট:
```
Hello, world!
```

একটি কম্যান্ডের আউটপুট স্ট্রিং এর মধ্যে অন্তর্ভুক্ত করতে:

```fish
echo "I have (count (ls)) files in this directory."
```

আউটপুট হতে পারে:
```
I have 9 files in this directory.
```

ভেরিয়েবল এবং কম্যান্ড মূল্যায়ন করা হয় এবং যেখানে তাদের রাখা হয় সেখানে নিখুঁতভাবে সন্নিবেশ করা হয়।

## গভীর ডুব
Fish এবং অন্যান্য আধুনিক শেলের আগে, আপনি প্রায়শই কোটেশন এবং কনক্যাটেনেশনের একটি জটিল সমন্বয় ব্যবহার করতেন—অথবা স্ট্রিংসে ভেরিয়েবল পেতে বাহ্যিক টুলসের উপর নির্ভর করতেন।

উদাহরণস্বরূপ, bash এ এটি এরকম হবে:

```bash
name="world"
echo "Hello, "$name"!"
```

ততটা মসৃণ না, তাই না?

Fish শুধুমাত্র এই প্রক্রিয়াটি সহজ করে না, বরং ত্রুটি সংক্রান্ত অবস্থাগুলিও আরও সুন্দরভাবে সম্ভালায়। যদি কোনো ভেরিয়েবল না থাকে, Fish একটি খালি স্ট্রিং সন্নিবেশ করবে, ইন্টারপোলেশন ভুল সম্ভালনোর সম্ভাবনা কমিয়ে দেবে।

সরাসরি ইন্টারপোলেশন ব্যতীত অন্যান্য উপায় অন্তর্ভুক্তি `printf` কম্যান্ড ব্যবহার করা:

```fish
set animal "narwhal"
printf "The %s is an awesome creature!" $animal
```

আউটপুট:
```
The narwhal is an awesome creature!
```

এই ক্ষেত্রে, `%s` হল স্ট্রিং ভেরিয়েবল `$animal` এর জন্য একটি প্লেসহোল্ডার যা `printf` দ্বারা প্রতিস্থাপন করা হয়।

বাস্তবায়নের দিক থেকে, Fish কম্যান্ড লাইন প্রক্রিয়া করার সময়, এটি ডাবল-কোটেড স্ট্রিংগুলি পার্স করে এবং উড়ন্তে ভেরিয়েবলগুলির সাথে তাদের মান প্রতিস্থাপন করে। এটি মার্জিত এবং Ruby অথবা PHP এর মত উচ্চস্তরের ভাষাগুলিতে পাওয়া ভেরিয়েবল ইন্টারপোলেশনের অনুরূপ।

## আরও দেখুন
Fish স্ট্রিং ম্যানিপুলেশন এবং স্ক্রিপ্টিং সম্পর্কে আরও জানতে এগুলি দেখুন:

- [Fish Shell ডকুমেন্টেশন: কোটেশন](https://fishshell.com/docs/current/index.html#quotes)
- [Fish Shell টিউটোরিয়াল](https://fishshell.com/docs/current/tutorial.html)
- [Stack Overflow: Fish এ কম্যান্ডে ভেরিয়েবল ব্যবহার কীভাবে করবেন](https://stackoverflow.com/questions/2763006/how-to-use-variables-in-a-command-in-fish)
