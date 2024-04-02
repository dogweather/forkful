---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:34.451054-06:00
description: "\u0986\u09B8\u09B2 \u098F\u09AC\u0982 \u0995\u09BE\u09B2\u09CD\u09AA\
  \u09A8\u09BF\u0995 \u0985\u0982\u09B6 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0997\u09A0\
  \u09BF\u09A4 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\
  \u09C1\u09B2\u09BF \u09AA\u09CD\u09B0\u0995\u09CC\u09B6\u09B2, \u09AA\u09A6\u09BE\
  \u09B0\u09CD\u09A5\u09AC\u09BF\u09A6\u09CD\u09AF\u09BE, \u098F\u09AC\u0982 \u09B8\
  \u09BF\u0997\u09A8\u09CD\u09AF\u09BE\u09B2 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE\u0995\u09B0\u09A3\u09C7\u09B0 \u09AE\u09A4 \u09AC\u09BF\u09AD\
  \u09BF\u09A8\u09CD\u09A8 \u0997\u09A3\u09A8\u09BE \u0995\u09CD\u09B7\u09C7\u09A4\
  \u09CD\u09B0\u09C7 \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u0987\u2026"
lastmod: '2024-03-17T18:47:44.075901-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B8\u09B2 \u098F\u09AC\u0982 \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\
  \u09BF\u0995 \u0985\u0982\u09B6 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0997\u09A0\u09BF\
  \u09A4 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\
  \u09B2\u09BF \u09AA\u09CD\u09B0\u0995\u09CC\u09B6\u09B2, \u09AA\u09A6\u09BE\u09B0\
  \u09CD\u09A5\u09AC\u09BF\u09A6\u09CD\u09AF\u09BE, \u098F\u09AC\u0982 \u09B8\u09BF\
  \u0997\u09A8\u09CD\u09AF\u09BE\u09B2 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\
  \u09BC\u09BE\u0995\u09B0\u09A3\u09C7\u09B0 \u09AE\u09A4 \u09AC\u09BF\u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u0997\u09A3\u09A8\u09BE \u0995\u09CD\u09B7\u09C7\u09A4\u09CD\
  \u09B0\u09C7 \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987\
  \u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কি এবং কেন?

আসল এবং কাল্পনিক অংশ নিয়ে গঠিত জটিল সংখ্যাগুলি প্রকৌশল, পদার্থবিদ্যা, এবং সিগন্যাল প্রক্রিয়াকরণের মত বিভিন্ন গণনা ক্ষেত্রে অপরিহার্য। প্রোগ্রামাররা এই সংখ্যাগুলি ব্যবহার করে এমন সমীকরণ সমাধান করেন যা আসল সংখ্যা দ্বারা করা যায় না, যেমন নেগেটিভ সংখ্যার মূল খুঁজে পাওয়া।

## কিভাবে:

হাস্কেল `Data.Complex` মডিউল ব্যবহার করে জটিল সংখ্যাগুলো নিয়ন্ত্রণ করে। এখানে একটি দ্রুত পর্যালোচনা দেওয়া হল:

```haskell
import Data.Complex

-- দুটি জটিল সংখ্যা সংজ্ঞায়িত করা
let z1 = 3 :+ 4  -- অর্থাৎ 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- অঙ্কগণিত অপারেশনগুলি
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- জটিল সংযুক্ত
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- বৃহত্ত্ব এবং ফেজ
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- ধ্রুবক থেকে আয়তক্ষেত্রিক রূপান্তর এবং তার বিপরীত
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- z1 এর মতো সমান
```

GHCi-এ উপরের কোড লোড করার পর নমুনা আউটপুট হতে পারে:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## গভীর ডুব

জটিল সংখ্যাগুলো 16 শতকে ফিরে যায় কিন্তু অনেক পরে ব্যপকভাবে গৃহীত হয়। হাস্কেলের মতো অনেক ভাষা জটিল অঙ্ক সমাধানে জন্মজাত সমর্থন প্রদান করে, মৌলিক গণিত প্রয়োগ না করেই এই সংখ্যাগুলিকে সহজে কাজে লাগানো সম্ভব করে।

বিকল্প অন্তর্ভুক্ত করে আপনার নিজের জটিল সংখ্যার প্রকার বানানো বা 3D গ্রাফিক্সের জন্য quaternions মতো নির্দিষ্ট ডোমেইনের জন্য লাইব্রেরি ব্যবহার করা। কিন্তু বেশিরভাগ ব্যবহারের ক্ষেত্রে, Haskell-এর `Data.Complex` পর্যাপ্ত।

অন্তর্নিহিতভাবে, `Data.Complex` কেবল দুটি `Float` বা `Double` মানের একটি ডেটা টাইপ জুটি, যা যথাক্রমে আসল এবং কাল্পনিক অংশগুলি প্রতিনিধিত্ব করে। এটি হাস্কেল প্ল্যাটফর্মে জটিল সংখ্যা নিয়ে কাজ করার একটি সোজা এবং দক্ষ উপায়।

## দেখুন অন্যান্য

Haskell-এ জটিল সংখ্যার বিষয়ে আরও জানার জন্য এই সম্পদগুলিতে দেখুন:

- Haskell `Data.Complex` রেসমি ডকুমেন্টেশন: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Haskell-এর সংখ্যা প্রকারগুলি সম্বন্ধে গভীর ডুব: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- একটি অ্যাপ্লিকেশনের জন্য, Haskell-এ Fast Fourier Transform অ্যালগরিদম অন্বেষণ করুন: [Haskell FFT library](https://hackage.haskell.org/package/fft)
