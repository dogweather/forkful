---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:51.088304-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B0\u09BE\u0989\u09A8\u09CD\u09A1\
  \u09BF\u0982 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09A6\u09B6\u09AE\u09BF\u0995\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u098F\u09B0 \u09A8\u09BF\u0995\
  \u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\u09A3 \u09AE\u09BE\u09A8\u09C7 \u09AC\
  \u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u0982\u0996\
  \u09CD\u09AF\u0995 \u09A6\u09B6\u09AE\u09BF\u0995 \u0985\u0999\u09CD\u0995\u09C7\
  \ \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u099C\u099F\u09BF\u09B2\u09A4\u09BE \u09B9\u09CD\u09B0\u09BE\u09B8 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AA\u09A0\u09A8\u09C0\u09AF\u09BC\u09A4\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:43.943147-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B0\u09BE\u0989\u09A8\u09CD\u09A1\
  \u09BF\u0982 \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09A6\u09B6\u09AE\u09BF\u0995\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0995\u09C7 \u098F\u09B0 \u09A8\u09BF\u0995\
  \u099F\u09A4\u09AE \u09AA\u09C2\u09B0\u09CD\u09A3 \u09AE\u09BE\u09A8\u09C7 \u09AC\
  \u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u0982\u0996\
  \u09CD\u09AF\u0995 \u09A6\u09B6\u09AE\u09BF\u0995 \u0985\u0999\u09CD\u0995\u09C7\
  \ \u09B8\u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u099C\u099F\u09BF\u09B2\u09A4\u09BE \u09B9\u09CD\u09B0\u09BE\u09B8 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AA\u09A0\u09A8\u09C0\u09AF\u09BC\u09A4\
  \u09BE\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
---

{{< edit_this_page >}}

## কি এবং কেন?

সংখ্যা রাউন্ডিং হল একটি দশমিক সংখ্যাকে এর নিকটতম পূর্ণ মানে বা নির্দিষ্ট সংখ্যক দশমিক অঙ্কে সামঞ্জস্য করা। প্রোগ্রামাররা জটিলতা হ্রাস করার জন্য, পঠনীয়তা উন্নতির জন্য, অথবা নির্ভুলতা প্রয়োজনীয়তার সাথে মেলে ধরার জন্য সংখ্যা রাউন্ড করে।

## কিভাবে:

এল্মের `Basics` মডিউলে রাউন্ডিং এর জন্য কার্যকরী ফাংশনস রয়েছে: `round`, `floor`, এবং `ceiling`। এগুলো ব্যবহারের উপায় নিচে দেওয়া হোলো।

```elm
import Basics exposing (round, floor, ceiling)

-- নিকটতম পূর্ণ সংখ্যায় রাউন্ড করা
round 3.14    --> 3
round 3.5     --> 4

-- নিচের দিকে রাউন্ড করা
floor 3.999   --> 3

-- উপরের দিকে রাউন্ড করা
ceiling 3.001 --> 4

-- রাউন্ড ছাড়াই দশমিক বিন্দু ট্রাঙ্কেট করা
truncate 3.76 --> 3
```

এল্ম নির্দিষ্ট সংখ্যক দশমিক অঙ্কে রাউন্ড করার জন্য `toLocaleString` ফাংশন প্রদান করেঃ

```elm
import Float exposing (toLocaleString)

-- দুই দশমিক অঙ্কে রাউন্ড করা
toLocaleString 2 3.14159 --> "3.14"
```

## গভীর ডুব

এল্ম একটি দৃঢ়ভাবে টাইপ-করা ফাংশনাল ভাষা যা পার্শ্ব প্রভাবগুলিকে আর্কিটেকচারের "প্রান্তে" সীমাবদ্ধ করে। অর্থাৎ রাউন্ডিং এর মতো ফাংশনগুলো নির্মল এবং পূর্বানুমানযোগ্য হতে হবে। ঐতিহাসিকভাবে, রাউন্ডিং অনেক প্রোগ্রামিং ভাষায় একটি সাধারণ অপারেশন, যা ভাসমান-বিন্দু অঙ্কনীতির অসংখ্যতার সাথে মোকাবিলা করে।

এল্মের রাউন্ডিং পদ্ধতিটি সরল - ফাংশনগুলি নির্মল এবং গণিতের সংজ্ঞাগুলির সাথে মিলে যায়। এল্ম বিল্ট-ইন ফাংশন প্রদান করে, যা নির্ভুলতা ব্যবস্থাপনার একটি ঘন ঘন প্রয়োজন, বিশেষ করে অর্থনীতি এবং গ্রাফিক্সে।

এল্মের বিল্ট-ইন ফাংশনগুলির বিকল্পের মধ্যে অঙ্কিক অপারেশন ব্যবহার করে কাস্টম বাস্তবায়ন অন্তর্ভুক্ত হতে পারে, তবে যখন মানক লাইব্রেরি কাজটি কার্যকরীভাবে করে তখন অযথা জটিলতা যোগ করা হয়।

বর্তমান সংস্করণে, এল্ম এই অপারেশনগুলির জন্য জাভাস্ক্রিপ্টের অধীনস্থ ভাসমান-বিন্দু গণিত ব্যবহার করে, এবং আইইইই ৭৫৪ মানদণ্ডের সাথে সঙ্গতি রেখে চলে, যা নির্ভুলতা এবং সম্ভাব্য ভাসমান-বিন্দু ত্রুটিগুলি বিবেচনা করার সময় মনে রাখা দরকার।

## আরও দেখুন

- এল্মের অফিসিয়াল `Basics` মডিউল ডকুমেন্টেশন: https://package.elm-lang.org/packages/elm/core/latest/Basics
- কম্পিউটিং-এ ভাসমান-বিন্দু সংখ্যার কাজের গভীরে একটি বিশ্লেষণ: https://floating-point-gui.de/
- আরও ভাসমান-বিন্দু অপারেশনের জন্য এল্মের `Float` মডিউল: https://package.elm-lang.org/packages/elm/core/latest/Float
