---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:16.571005-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Elm-\u098F \u099C\u099F\u09BF\u09B2\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\
  \u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\
  \u09C7\u0987, \u09A4\u09BE\u0987 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A8\u09BF\
  \u099C\u09C7\u09B0 \u099F\u09BE\u0987\u09AA \u098F\u09AC\u0982 \u09AB\u09BE\u0982\
  \u09B6\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u09B8\u09C7\u099F\u0986\u09AA \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\
  \u09B2."
lastmod: '2024-03-17T18:47:43.942225-06:00'
model: gpt-4-0125-preview
summary: "Elm-\u098F \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\
  \u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u09A4\u09BE\u0987 \u0986\
  \u09AA\u09A8\u09BE\u0995\u09C7 \u09A8\u09BF\u099C\u09C7\u09B0 \u099F\u09BE\u0987\
  \u09AA \u098F\u09AC\u0982 \u09AB\u09BE\u0982\u09B6\u09A8 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u09B8\u09C7\u099F\u0986\
  \u09AA \u09A6\u09C7\u0993\u09DF\u09BE \u09B9\u09B2."
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
