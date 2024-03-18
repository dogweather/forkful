---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:14.441769-06:00
description: "PHP \u09A4\u09C7 \u09AF\u09BE\u09A6\u09C3\u099A\u09CD\u099B\u09BF\u0995\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u09AA\u09B0\u09BF\u09B8\u09B0\u09C7 \u0985\u09A8\u09C1\u09AE\u09BE\u09A8\u09BE\
  \u09A4\u09C0\u09A4 \u09AE\u09BE\u09A8 \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\
  \u09B0\u09BE, \u09AF\u09BE \u0985\u09A8\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0 \u0986\u0987\u09A1\u09BF \u09A4\u09C8\
  \u09B0\u09BF, \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u099C\
  \u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\
  \ \u09B8\u09BF\u09AE\u09C1\u09B2\u09C7\u09B6\u09A8 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.125167-06:00'
model: gpt-4-0125-preview
summary: "PHP \u09A4\u09C7 \u09AF\u09BE\u09A6\u09C3\u099A\u09CD\u099B\u09BF\u0995\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u09AA\u09B0\u09BF\u09B8\u09B0\u09C7 \u0985\u09A8\u09C1\u09AE\u09BE\u09A8\u09BE\
  \u09A4\u09C0\u09A4 \u09AE\u09BE\u09A8 \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\
  \u09B0\u09BE, \u09AF\u09BE \u0985\u09A8\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0 \u0986\u0987\u09A1\u09BF \u09A4\u09C8\
  \u09B0\u09BF, \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u099C\
  \u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE \u0985\u09A5\u09AC\u09BE\
  \ \u09B8\u09BF\u09AE\u09C1\u09B2\u09C7\u09B6\u09A8 \u098F\u09AC\u0982\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

PHP তে যাদৃচ্ছিক সংখ্যা তৈরি করা মানে নির্দিষ্ট পরিসরে অনুমানাতীত মান উৎপন্ন করা, যা অনন্য ব্যবহারকারী আইডি তৈরি, পাসওয়ার্ড জেনারেট করা অথবা সিমুলেশন এবং গেমে ব্যবহারের মতো কাজের জন্য অপরিহার্য। প্রোগ্রামাররা তাদের অ্যাপ্লিকেশনে অদৃশ্যতা এবং বৈচিত্র্য যুক্ত করতে যাদৃচ্ছিকতার উপর নির্ভর করে থাকেন, যা পরীক্ষা বা ব্যবহারকারীর অভিজ্ঞতাকে আরও দৃঢ় এবং আকর্ষণীয় করে তোলে।

## কিভাবে:

PHP যাদৃচ্ছিক সংখ্যা তৈরির জন্য কয়েকটি ফাংশন প্রদান করে, তবে সবচেয়ে সাধারণভাবে ব্যবহৃত হল `rand()`, `mt_rand()`, এবং ক্রিপ্টোগ্রাফিক উদ্দেশ্যে `random_int()`।

0 থেকে getrandmax() (যা `rand()` দ্বারা ফেরত দেওয়া সবচেয়ে বড় সম্ভাব্য মান) পর্যন্ত একটি সাধারণ যাদৃচ্ছিক সংখ্যা তৈরি করতে আপনি ব্যবহার করতে পারেন:

```PHP
echo rand();
```

আরও নির্দিষ্ট পরিসরের জন্য, যেমন 1 থেকে 100 পর্যন্ত:

```PHP
echo rand(1, 100);
```

তবে, গতি এবং যাদৃচ্ছিকতার জন্য `mt_rand()` অধিক ভালো পছন্দ:

```PHP
echo mt_rand(1, 100);
```

উভয়ের আউটপুট যাদৃচ্ছিকভাবে 1 থেকে 100 এর মধ্যে কিছু হতে পারে, যেমন `42`.

ক্রিপ্টোগ্রাফিক বা নিরাপত্তা প্রসঙ্গে, যেখানে অনুমানাতীত হওয়া অপরিহার্য, `random_int()` পছন্দনীয় পছন্দ কারণ এটি ক্রিপ্টোগ্রাফিকভাবে নিরাপদ প্সিউডো-যাদৃচ্ছিক পূর্ণসংখ্যা উৎপন্ন করে:

```PHP
echo random_int(1, 100);
```

আবার, আউটপুট যাদৃচ্ছিকভাবে 1 থেকে 100 এর মধ্যে হতে পারে, যেমন `84`, কিন্তু ইহা যাদৃচ্ছিকতার একটি শক্তিশালী গ্যারান্টি দেয়।

## গভীরে ডুব:

`rand()` ফাংশন PHP এর প্রাথমিক সংস্করণ থেকে রয়েছে, যাদৃচ্ছিক সংখ্যা তৈরির প্রাথমিক পদ্ধতি হিসেবে কাজ করে। তবে, উচ্চ মাত্রার যাদৃচ্ছিকতা প্রয়োজন হলে এটি সর্বোত্তম পছন্দ নয় এর তুলনামূলকভাবে পূর্বানুমেয় অ্যালগরিদমের কারণে।

PHP 4 এ `mt_rand()` চালু হয়, যা মার্সেন টুইস্টার অ্যালগরিদমের ওপর ভিত্তি করে - গতি এবং এর তৈরি করতে পারে এমন যাদৃচ্ছিকতার দিক দিয়ে `rand()` এর চেয়ে অনেক শ্রেষ্ঠ। অচিরেই এটি অ-ক্রিপ্টোগ্রাফিক প্রয়োজনীয়তার জন্য পছন্দনীয় বিকল্প হয়ে উঠেছিল।

নিরাপত্তা-সংবেদনশীল অ্যাপ্লিকেশনের জন্য, PHP 7 এ `random_int()` চালু হয়েছিল যা সিস্টেমের যাদৃচ্ছিক নম্বর জেনারেটর থেকে যাদৃচ্ছিক বাইটস ব্যবহার করে ক্রিপ্টোগ্রাফিকভাবে নিরাপদ প্সিউডো-যাদৃচ্ছিক পূর্ণসংখ্যা উৎপন্ন করে। এটি `rand()` বা `mt_rand()` এর চেয়ে অনেক বেশি নিরাপদ, তাই টোকেন, চাবি, বা অন্যান্য উপাদান তৈরির জন্য, যেখানে অনুমানাতীতা নিরাপত্তা দুর্বলতার কারণ হতে পারে, এটি সর্বোত্তম পছন্দ।

এই উন্নতিসমূহের পরেও, অ্যাপ্লিকেশনের প্রেক্ষাপট বুঝে সঠিক ফাংশন নির্বাচন করা অপরিহার্য। সাধারণ ব্যবহারের জন্য `mt_rand()` যথেষ্ট, তবে যা কিছু লক্ষ্য বা শোষণ করা হতে পারে, তার জন্য `random_int()` পথ অনুসরণ করে, যাদৃচ্ছিকতা এবং নিরাপত্তা উভয়ই প্রদান করে।