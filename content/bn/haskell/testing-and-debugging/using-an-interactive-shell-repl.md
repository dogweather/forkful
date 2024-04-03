---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:34.744865-06:00
description: "\u09B9\u09BE\u09B8\u09CD\u0995\u09C7\u09B2\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u09BF\u09AD \u09B6\u09C7\u09B2 \u09AC\u09BE REPL (\u09AA\u09A1\u09BC\u09BE-\u09AE\
  \u09C2\u09B2\u09CD\u09AF\u09BE\u09DF\u09A3-\u09AE\u09C1\u09A6\u09CD\u09B0\u09A3\
  \ \u09B2\u09C1\u09AA) \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B2\u09BE\u0987\u09AD\
  \ \u0995\u09CB\u09A1 \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F \u099A\u09BE\u09B2\
  \u09BE\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\u09BE \u09A6\u09CD\
  \u09B0\u09C1\u09A4 \u09AA\u09CD\u09B0\u09A4\u09BF\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \u09BE, \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.084857-06:00'
model: gpt-4-0125-preview
summary: "\u09B9\u09BE\u09B8\u09CD\u0995\u09C7\u09B2\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u09BF\u09AD \u09B6\u09C7\u09B2 \u09AC\u09BE REPL (\u09AA\u09A1\u09BC\u09BE-\u09AE\
  \u09C2\u09B2\u09CD\u09AF\u09BE\u09DF\u09A3-\u09AE\u09C1\u09A6\u09CD\u09B0\u09A3\
  \ \u09B2\u09C1\u09AA) \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B2\u09BE\u0987\u09AD\
  \ \u0995\u09CB\u09A1 \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F \u099A\u09BE\u09B2\
  \u09BE\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\u09BE \u09A6\u09CD\
  \u09B0\u09C1\u09A4 \u09AA\u09CD\u09B0\u09A4\u09BF\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \u09BE, \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AD\u09BE\u09B7\u09BE \u09B6\u09C7\u0996\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u0996\u09C7\u09B2\
  \u09BE\u09B0 \u09AE\u09BE\u09A0\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
GHCi (গ্লাসগো হাস্কেল কম্পাইলারের ইন্টার‌্যাক্টিভ পরিবেশ) শুরু করতে, কেবল আপনার টার্মিনালে `ghci` টাইপ করুন। এটি কিভাবে ব্যবহার করবেন তা নিচে দেওয়া হল:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

নমুনা আউটপুট বুঝায় যে `x` একটি সংখ্যাসূচক চলক এবং দেখায় যে এটি দ্বিগুণ করলে ফলাফল ১০ হয়।

## গভীর ডুব:
হাস্কেলের GHCi এর সূচনা থেকে বেশ অনেক পথ এসেছে। এটি ট্যাব সম্পূর্ণ, মাল্টি-লাইন ইনপুট, এবং প্যাকেজ লোডিং এর মত সমৃদ্ধ সেট অফ ফিচার সরবরাহ করে। Hugs এর মত বিকল্পগুলি এখন বেশিরভাগ ঐতিহাসিক, GHCi স্ট্যান্ডার্ড হয়ে উঠেছে। GHCi প্রতিবার আপনি একটি প্রকাশ লিখেন ঠিক তখনই কোড কম্পাইল করে, যা আপনাকে আপনার হাস্কেল কোড পরীক্ষা করার একটি দক্ষ উপায় দেয়।

## দেখুন আরও:
- [জিএইচসি ইউজারের গাইড – জিএইচসিআই](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [লার্ন ইউ আ হাস্কেল ফর গ্রেট গুড! – শুরু করা](http://learnyouahaskell.com/starting-out#hello-world)
- [হাস্কেল উইকি – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
