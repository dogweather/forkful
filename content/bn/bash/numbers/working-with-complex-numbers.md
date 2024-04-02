---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:46.636269-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B0\
  \u09BF\u09AF\u09BC\u09C7\u09B2 \u09AA\u09BE\u09B0\u09CD\u099F \u098F\u09AC\u0982\
  \ \u098F\u0995\u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u09AA\
  \u09BE\u09B0\u09CD\u099F \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0997\u09A0\u09BF\
  \u09A4\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u09B8\u09BF\u0997\u09A8\u09CD\u09AF\u09BE\u09B2 \u09AA\u09CD\u09B0\u0995\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3, \u0995\u09CB\u09AF\u09BC\
  \u09BE\u09A8\u09CD\u099F\u09BE\u09AE \u09AE\u09C7\u0995\u09BE\u09A8\u09BF\u0995\u09CD\
  \u09B8 \u098F\u09AC\u0982 \u09AF\u0996\u09A8\u0987 \u0997\u09A3\u09A8\u09BE \u09A4\
  \u09BE\u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\
  \u09AF\u09BC,\u2026"
lastmod: '2024-03-17T18:47:44.217492-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09B0\u09BF\
  \u09AF\u09BC\u09C7\u09B2 \u09AA\u09BE\u09B0\u09CD\u099F \u098F\u09AC\u0982 \u098F\
  \u0995\u099F\u09BF \u0995\u09BE\u09B2\u09CD\u09AA\u09A8\u09BF\u0995 \u09AA\u09BE\
  \u09B0\u09CD\u099F \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0997\u09A0\u09BF\u09A4\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09B8\u09BF\u0997\u09A8\u09CD\u09AF\u09BE\u09B2 \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE\u0995\u09B0\u09A3, \u0995\u09CB\u09AF\u09BC\u09BE\
  \u09A8\u09CD\u099F\u09BE\u09AE \u09AE\u09C7\u0995\u09BE\u09A8\u09BF\u0995\u09CD\u09B8\
  \ \u098F\u09AC\u0982 \u09AF\u0996\u09A8\u0987 \u0997\u09A3\u09A8\u09BE \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AF\
  \u09BC,\u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কি এবং কেন?
জটিল সংখ্যা রিয়েল পার্ট এবং একটি কাল্পনিক পার্ট দ্বারা গঠিত। প্রোগ্রামাররা সিগন্যাল প্রক্রিয়াকরণ, কোয়ান্টাম মেকানিক্স এবং যখনই গণনা তাদের প্রয়োজন হয়, তখন তারা এগুলিকে ব্যবহার করে, কারণ সাধারণ রিয়েল সংখ্যা এই দরকারগুলি পূরণ করে না।

## কিভাবে:
ব্যাশ সরাসরি জটিল সংখ্যা সমর্থন করে না। তুমি প্রায়ই `bc` এর মতো একটি বাহ্যিক টুল ব্যবহার করবে এর `-l` অপশনের সাথে। এখানে ব্যাশ এ জটিল সংখ্যা নিয়ে কাজ করার উপায় দেওয়া হল:

```bash
echo "sqrt(-1)" | bc -l
```

আউটপুট:
```bash
j
```

গুণন:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

আউটপুট:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## গভীর ডাইভ
জটিল সংখ্যা 16শ শতাব্দী থেকে আছে, কিন্তু ব্যাশের মতো স্ক্রিপ্টিং ভাষা জটিল সংখ্যা মতো গাণিতিক গণনা জন্য প্রস্তুত নয়। এই কারণে `bc` বা `awk` এর মতো অন্যান্য টুল প্রায়ই খেলায় আসে। জটিল সংখ্যা সহ কাজ করার জন্য কিছু বিকল্প ভাষা হলো পাইথন এর `cmath` মডিউল এবং ম্যাটল্যাব, যা উভয়ই আরও উন্নত গাণিতিক ফাংশনের জন্য নির্মিত। ব্যাশের ক্ষেত্রে, এটি সব টুলস ব্যবহার করে - `bc` কাল্পনিক ইউনিট প্রকাশ করতে ছোট 'i' ব্যবহার করে এবং যোগ, বিয়োগ, গুণন, এবং ভাগের মতো মৌলিক অপারেশন সমর্থন করে।

## দেখতে পারো
- বিসি ম্যানুয়াল: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU অক্টেভ (ম্যাটল্যাবের বিকল্প): https://www.gnu.org/software/octave/
- পাইথন `cmath` মডিউল: https://docs.python.org/3/library/cmath.html
