---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:05.390432-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\u09BE\u0995\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u0986\u09B8\u09B2\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09AE\u09BE\u09A8\u0995\u09C7 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\
  \u09A4\u09BE\u09AF\u09BC \u0985\u09A8\u09C1\u09AE\u09BE\u09A8 \u0995\u09B0\u09BE\
  , \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u0985\u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u09A6\u09B6\u09AE\u09BF\u0995\u0997\u09C1\u09B2\
  \u09BF \u09A4\u09C1\u09B2\u09C7 \u09A6\u09BF\u09A4\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AE\u09C7\u09AE\u09B0\
  \u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.403874-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\u09BE\u0995\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u0986\u09B8\u09B2\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09AE\u09BE\u09A8\u0995\u09C7 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\
  \u09A4\u09BE\u09AF\u09BC \u0985\u09A8\u09C1\u09AE\u09BE\u09A8 \u0995\u09B0\u09BE\
  , \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u0985\u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u09A6\u09B6\u09AE\u09BF\u0995\u0997\u09C1\u09B2\
  \u09BF \u09A4\u09C1\u09B2\u09C7 \u09A6\u09BF\u09A4\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AE\u09C7\u09AE\u09B0\
  \u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE,\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কি এবং কেন?

সংখ্যা গোলাকার করা আসলে একটি সংখ্যার মানকে নির্দিষ্ট নির্ভুলতায় অনুমান করা, সাধারণত অপ্রয়োজনীয় দশমিকগুলি তুলে দিতে। প্রোগ্রামাররা মেমরি পরিচালনা, পঠনীয়তা উন্নতি এবং মুদ্রা বাধ্যবাধকতা জাতীয় ডোমেন-নির্দিষ্ট প্রয়োজনে পূরণের মতো বিষয় সমূহের জন্য সংখ্যা গোলাকার করে।

## কিভাবে:

Swift সংখ্যা গোলাকার করার জন্য বিভিন্ন উপায় সরবরাহ করে। এখানে একটি উদাহরণ দেওয়া হল:

```Swift
let original = 3.14159

// স্ট্যান্ডার্ড গোলাকার করা
let standardRounded = round(original) // 3.0

// নির্দিষ্ট দশমিক স্থানে গোলাকার করা
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// নীচের দিকে গোলাকার করা
let roundedDown = floor(original) // 3.0

// উপরের দিকে গোলাকার করা
let roundedUp = ceil(original) // 4.0

print("স্ট্যান্ডার্ড: \(standardRounded), দশমিক: \(decimalRounded), নিচে: \(roundedDown), উপরে: \(roundedUp)")
```

আউটপুট: `স্ট্যান্ডার্ড: 3.0, দশমিক: 3.142, নিচে: 3.0, উপরে: 4.0`

## গভীর ডুব

ঐতিহাসিকভাবে, গোলাকার করা একটি গাণিতিক ধারণা যা কম্পিউটারের আগে থেকেই বাণিজ্য ও বিজ্ঞানে অপরিহার্য। Swift-এর `Foundation` ফ্রেমওয়ার্ক ব্যাপক গোলাকার করার কার্যকারিতা সরবরাহ করে:

- `round(_: )` হল সেই পুরনো হাফ-আপ গোলাকার করার পদ্ধতি।
- `floor(_: )` এবং `ceil(_: )` দিকনির্দেশ গোলাকার করার জন্য পরিচালনা করে।
- `rounded(.up/.down/.toNearestOrAwayFromZero)` গোলাকার করার নীতির এনামের সাথে আরও নিখুঁত নিয়ন্ত্রণ দেয়।

সঠিক আর্থিক হিসাবের জন্য `Decimal` টাইপ সম্পর্কে সচেতন থাকুন, যা ফ্লোটিং-পয়েন্টের ত্রুটিগুলি এড়ায়। এছাড়াও, Objective-C সামঞ্জস্যের জন্য `NSDecimalNumber` অন্বেষণ করুন।

## আরও দেখুন

- ফ্লোটিং-পয়েন্ট অঙ্কের জন্য IEEE স্ট্যান্ডার্ড (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
