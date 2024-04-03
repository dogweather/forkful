---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:02.530169-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF \u099C\
  \u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09AF\u09BC\
  \u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u0995\u09B0\u09C7\
  \ \u09A6\u09C7\u09AF\u09BC\u0964 \u0986\u09AA\u09A8\u09BF \u09A4\u09BE\u09A6\u09C7\
  \u09B0 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\u099A\u09BE\
  \u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u0995\
  \u09AE\u09AA\u09CD\u09B2\u09C7\u0995\u09CD\u09B8 \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7."
lastmod: '2024-03-17T18:47:44.581098-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\
  \u09AF\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u0986\u09AA\
  \u09A8\u09BF \u09A4\u09BE\u09A6\u09C7\u09B0 \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\
  \u0982 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8 \u0995\u09AE\u09AA\u09CD\u09B2\u09C7\u0995\u09CD\
  \u09B8 \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
রুবি জটিল সংখ্যা নিয়ে কাজ করা সহজ করে দেয়। আপনি তাদের তৈরি এবং পরিচালনা করতে পারেন কমপ্লেক্স ক্লাসের মাধ্যমে:

```ruby
require 'complex'

# জটিল সংখ্যা তৈরি করা
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# মৌলিক অপারেশন
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# সংযোগ, পরিমাণ, এবং ফেজ
conjugate = c1.conjugate    # => (3.0-4.0i)
magnitude = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 রেডিয়ান

# জটিল-বিশেষ পদ্ধতি
polar = c1.polar            # => [5.0, 0.9272952180016122]
rectangular = c1.rect       # => [3.0, 4.0]
```

## গভীরে ডুব:
জটিল সংখ্যা নতুন নয়—এরা ১৬তম শতাব্দী থেকে আছে, মূলত সেসব সমীকরণ সমাধানে যার বাস্তব সমাধান নেই। গণিত বাদে, কম্পিউটেশনালভাবে, রুবির কমপ্লেক্স ক্লাস গুরুত্বপূর্ণ কাজ করে, যা ম্যাথ মডিউল দ্বারা ত্রিকোণমিতিক এবং উৎক্রমণ ফাংশনের জন্য সমর্থিত।

পূর্ববর্তী প্রোগ্রামিং ভাষাগুলি বাস্তব এবং কল্পনামূলক অংশগুলি ম্যানুয়ালি সামলানোর প্রয়োজন ছিল। কিছু ভাষা, যেমন ফোরট্রান এবং C++, জটিল অঙ্কের জন্য বিশেষ লাইব্রেরিগুলি উৎসর্গ করে।

রুবির পদ্ধতি তার সিনট্যাক্সে জটিল সংখ্যা সমর্থন এম্বেড করে, যা আপনাকে চাকা পুনর্নির্মাণ করা থেকে মুক্তি দেয়। পর্দার পিছনে, কমপ্লেক্স ক্লাস গণিতের কাজ সামলায়, যেখানে রুবি অবজেক্ট ইন্টারঅ্যাকশনের যত্ন নেয়।

## আরো দেখুন
- রুবি ডকস অন কমপ্লেক্স: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- ম্যাথওয়ার্ল্ডের জটিল সংখ্যা সম্পর্কে: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- জটিল সংখ্যা এবং তারা কেন উপকারী তার দৃশ্যমান ভূমিকা: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
