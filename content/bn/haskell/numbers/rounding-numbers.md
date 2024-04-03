---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:36.688209-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AA\u09C2\u09B0\u09CD\u09A3\u09BE\
  \u0999\u09CD\u0997 \u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09A6\u09B6\u09AE\u09BF\u0995 \u09B8\u09CD\u09A5\u09BE\u09A8\u09C7 \u09B8\
  \u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF \u0995\u09B0\u09BE\u0995\u09C7\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B0\u09BE\u0989\u09A8\u09CD\u09A1\u09BF\
  \u0982 \u09AC\u09B2\u09BE \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A8\u09BF\u0996\u09C1\u0981\u09A4\
  \u09A4\u09BE \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3 \u0995\
  \u09B0\u09A4\u09C7, \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\
  \u09C0\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.076988-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AA\u09C2\u09B0\u09CD\u09A3\u09BE\
  \u0999\u09CD\u0997 \u09AC\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09A6\u09B6\u09AE\u09BF\u0995 \u09B8\u09CD\u09A5\u09BE\u09A8\u09C7 \u09B8\
  \u09BE\u09AE\u099E\u09CD\u099C\u09B8\u09CD\u09AF \u0995\u09B0\u09BE\u0995\u09C7\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B0\u09BE\u0989\u09A8\u09CD\u09A1\u09BF\
  \u0982 \u09AC\u09B2\u09BE \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A8\u09BF\u0996\u09C1\u0981\u09A4\
  \u09A4\u09BE \u09A8\u09BF\u09AF\u09BC\u09A8\u09CD\u09A4\u09CD\u09B0\u09A3 \u0995\
  \u09B0\u09A4\u09C7, \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\
  \u09C0\u09B0 \u0989\u09AA\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09CB\u0995\
  \u09C7 \u09B8\u09BE\u099C\u09BE\u09A4\u09C7, \u0985\u09A5\u09AC\u09BE \u09AD\u09BE\
  \u09B8\u09AE\u09BE\u09A8-\u09AC\u09BF\u09A8\u09CD\u09A6\u09C1 \u0985\u09AA\u09BE\
  \u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0997\u09A3\u09A8\u09BE \u0996\u09B0\u099A \u0995\u09AE\u09BE\u09A4\u09C7 \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE \u09B0\u09BE\u0989\u09A8\u09CD\u09A1 \u0995\u09B0\
  \u09C7\u0964."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কিভাবে:
হাস্কেল `Prelude`-এ রাউন্ডিং অপারেশনের জন্য `round`, `ceiling`, `floor`, এবং `truncate` ফাংশন ব্যবহার করে।

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- নির্দিষ্ট দশমিক স্থানে রাউন্ডিং করা Prelude-এ নেই।
  -- এখানে একটি কাস্টম ফাংশন:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## গভীর অনুসন্ধান
ঐতিহাসিকভাবে, সংখ্যাতাত্ত্বিক বিশ্লেষণ এবং কম্পিউটার বিজ্ঞানে রাউন্ডিং গুরুত্বপূর্ণ কারণ এটি গণনায় ত্রুটি সম্ভাবনা কমাতে গুরুত্বপূর্ণ, বিশেষ করে IEEE 754 এর সাথে ভাসমান-বিন্দু উপস্থাপনা মানকীকৃত হওয়ার আগে।

কোনটিকে রাউন্ড করা হবে? `round` আপনাকে নিকটতম পূর্ণাঙ্গে নিয়ে যায়—উপরে বা নীচে। `ceiling` এবং `floor` সবসময় যথাক্রমে নিকটতম পূর্ণাঙ্গে উপরে বা নীচে রাউন্ড করে, অন্যদিকে `truncate` কেবল দশমিক বিন্দুগুলি ফেলে দেয়।

এই ফাংশনগুলির বিকল্প হিসেবে কাস্টম লজিক, যেমন আমাদের `roundTo`, অথবা আরও জটিল দাবী পূরণের জন্য লাইব্রেরি (যেমন Data.Fixed) টেনে আনতে পারেন।

`round`-এ হাফ-ওয়ে কেসগুলি হাস্কেল কীভাবে হ্যান্ডেল করে তার জন্য অপ্রত্যাশিত ফলাফলের জন্য সাবধান থাকুন (এটি নিকটতম সম সংখ্যায় রাউন্ড করে)।

## আরও দেখুন
- রাউন্ডিং ফাংশনের জন্য হাস্কেল প্রিলুড ডকুমেন্টেশন: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- ভাসমান-বিন্দু অঙ্কের উপর হাস্কেল উইকি: https://wiki.haskell.org/Floating_point_arithmetic
- অনেক ভাষায় ভাসমান-বিন্দু কীভাবে সম্পন্ন করা হয় তা জানার জন্য IEEE 754-2008 মানদণ্ড: https://ieeexplore.ieee.org/document/4610935
