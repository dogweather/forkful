---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:16.571005-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B9\
  \u09B2 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u098F\u09AC\u0982 \u0995\u09B2\u09CD\
  \u09AA\u09BF\u09A4 (imaginary) \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09AE\u09A8\u09CD\u09AC\u09AF\u09BC, \u09AF\u09C7\u09AE\u09A8 `a + bi` \u09AF\u09C7\
  \u0996\u09BE\u09A8\u09C7 `i` \u09B9\u09B2 -1 \u098F\u09B0 \u09AC\u09B0\u09CD\u0997\
  \u09AE\u09C2\u09B2\u0964 \u0987\u099E\u09CD\u099C\u09BF\u09A8\u09BF\u09AF\u09BC\u09BE\
  \u09B0\u09BF\u0982 \u098F\u09AC\u0982 \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\u09AC\
  \u09BF\u099C\u09CD\u099E\u09BE\u09A8\u09C7\u09B0 \u09AE\u09A4\u09CB\u2026"
lastmod: '2024-03-17T18:47:43.942225-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B9\u09B2\
  \ \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u098F\u09AC\u0982 \u0995\u09B2\u09CD\u09AA\
  \u09BF\u09A4 (imaginary) \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\u09AE\
  \u09A8\u09CD\u09AC\u09AF\u09BC, \u09AF\u09C7\u09AE\u09A8 `a + bi` \u09AF\u09C7\u0996\
  \u09BE\u09A8\u09C7 `i` \u09B9\u09B2 -1 \u098F\u09B0 \u09AC\u09B0\u09CD\u0997\u09AE\
  \u09C2\u09B2\u0964 \u0987\u099E\u09CD\u099C\u09BF\u09A8\u09BF\u09AF\u09BC\u09BE\u09B0\
  \u09BF\u0982 \u098F\u09AC\u0982 \u09AA\u09A6\u09BE\u09B0\u09CD\u09A5\u09AC\u09BF\
  \u099C\u09CD\u099E\u09BE\u09A8\u09C7\u09B0 \u09AE\u09A4\u09CB \u0995\u09CD\u09B7\
  \u09C7\u09A4\u09CD\u09B0\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09A4\u09BE\u09B0\u09BE\
  \ \u0985\u09A4\u09CD\u09AF\u09A8\u09CD\u09A4 \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\
  \u09AC\u09AA\u09C2\u09B0\u09CD\u09A3, \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09B8\u09AE\
  \u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC \u09A8\u09BE\
  \ \u098F\u09AE\u09A8 \u09B8\u09AE\u09B8\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\
  \ \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u0964\
  ."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কীভাবে:
Elm-এ জটিল সংখ্যার জন্য নির্মিত সাপোর্ট নেই, তাই আপনাকে নিজের টাইপ এবং ফাংশন তৈরি করতে হবে। এখানে একটি দ্রুত সেটআপ দেওয়া হল:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- উদাহরণ ব্যবহার:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum হল { real = 4.0, imaginary = -2.0 }
```

## গভীর ডাইভ
ঐতিহাসিকভাবে, জটিল সংখ্যাগুলি সবসময় মেনে নেওয়া হত না। তারা 16 শতাব্দীতে ঘাতীক সমীকরণ সমাধানে একটি গেম-চেঞ্জার হয়ে উঠেছিল। পাইথনের মতো অন্যান্য ভাষার বিকল্পগুলি বাক্স থেকেই জটিল সংখ্যা সাপোর্ট এবং অপারেশনগুলি অফার করে। Elm তে আপনি যেমন দেখেছেন, একটি DIY পদ্ধতির প্রয়োজন হয়। তবে, আপনি এটিকে যতটা সম্ভব জটিল করে তুলতে পারেন, গুণ, ভাগ এবং অন্যান্য অপারেশনগুলি তৈরি করে, পারফরম্যান্স সমস্যা সমাধান করে।

## দেখতে পারেন
- Elm-এর অফিসিয়াল ডকুমেন্টেশন: https://package.elm-lang.org/ কাস্টম টাইপ তৈরি এবং Elm বেসিক্স মাস্টার করার জন্য।
- গণিতের ইতিহাস প্রেমীরা "An Imaginary Tale" দেখতে পারেন পল জে. নাহিনের লেখা, যা জটিল সংখ্যার সময়ের মধ্য দিয়ে যাত্রার কথা বলে।
- আপনার জটিল সংখ্যার যাদুকরী কৌশল প্রয়োগ করতে Project Euler (https://projecteuler.net) এ গণিত-ভিত্তিক প্রোগ্রামিং চ্যালেঞ্জে ডুব দিন।
