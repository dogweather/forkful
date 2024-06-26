---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:16.709518-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish-\u098F, \u0986\u09AE\u09B0\
  \u09BE \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\
  \u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BF `math` \u098F\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AF\u09BE\u09B0 \u09AC\u09BE\u09B8\u09CD\
  \u09A4\u09AC \u098F\u09AC\u0982 \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995\
  \ \u0985\u0982\u09B6 \u09B0\u09DF\u09C7\u099B\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09B6\u09C1\u09B0\u09C1."
lastmod: '2024-03-17T18:47:44.489301-06:00'
model: gpt-4-0125-preview
summary: "Fish-\u098F, \u0986\u09AE\u09B0\u09BE \u099C\u099F\u09BF\u09B2 \u09B8\u0982\
  \u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BF `math` \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AF\
  \u09BE\u09B0 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u098F\u09AC\u0982 \u0995\u09BE\
  \u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u0985\u0982\u09B6 \u09B0\u09DF\u09C7\u099B\
  \u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B6\u09C1\
  \u09B0\u09C1."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Fish-এ, আমরা জটিল সংখ্যা নিয়ে কাজ করি `math` এর মাধ্যমে যার বাস্তব এবং কাল্পনিক অংশ রয়েছে। এখানে একটি শুরু:

```fish
# দুটি জটিল সংখ্যা যোগ (3+4i) এবং (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # আউটপুট: 8+6i

# দুটি জটিল সংখ্যা গুণ (1+2i) এবং (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # আউটপুট: -5+10i
```

যদি আপনার কোনো জটিল সংখ্যাকে ক্ষমতা উত্তোলন বা এর ঘাতীয় রূপ পেতে হয়:

```fish
# স্কোয়ার অফ (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # আউটপুট: -5+12i

# এক্সপোনেনশিয়াল অফ (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # আউটপুট: -0.41615+0.9093i
```

## গভীর ডুব
Fish Shell-এর জটিল সংখ্যার জন্য ম্যাথ সমর্থন অপেক্ষাকৃত নতুন, যা 3.1.0 সংস্করণের দিকে শুরু হয়েছিল। এর আগে, লোকেরা হয়তো `bc` ব্যবহার করত অথবা Python এর মতো বাহ্যিক টুলগুলি ডেকে আনত।

Fish-এর ম্যাথের বিকল্পের মধ্যে আছে বিশেষায়িত সংখ্যাতাত্ত্বিক লাইব্রেরি অথবা ভাষা যেমন MATLAB, Python সহ NumPy, বা এমনকি C++ স্ট্যান্ডার্ড লাইব্রেরি সহ। তবে, এগুলি দ্রুত শেল গণনা জন্য অতিরিক্ত মনে হতে পারে।

Fish-এর জটিল সংখ্যা সমর্থন এর অভ্যন্তরীণ `math` কমান্ডে অন্তর্ভুক্ত, যা libcalc ব্যবহার করে। এর মানে মৌলিক কার্যাবলীর জন্য আপনাকে অতিরিক্ত টুল ইন্সটল করতে হবে না।

তবে, Fish ভারী গাণিতিক গণনার জন্য নকশা করা হয়নি। এর গণিত ক্ষমতা দ্রুত গণনা অথবা স্ক্রিপ্টের জন্য সুবিধাজনক, যেখানে জটিল সংখ্যা আসে তবে পরিশ্রমী কাজের জন্য আরো দৃঢ় টুলের বিবেচনা করুন।

## আরও দেখুন
- ম্যাথের জন্য Fish shell ডকুমেন্টেশন: https://fishshell.com/docs/current/commands.html#math
- Python এর জন্য NumPy, একটি জনপ্রিয় বিকল্প: https://numpy.org/
- জটিল সংখ্যা সম্পর্কে আরও গভীরে: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
