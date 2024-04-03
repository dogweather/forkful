---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:26.651486-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Bash \u098F \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0995\u09CD\u09AF\u09BE\
  \u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0995\u09CB\u09A8\u09CB\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\
  \u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\u09A8\u09BF \u09AA\
  \u09CD\u09AF\u09BE\u09B0\u09BE\u09AE\u09BF\u099F\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\
  \u09AA\u09CD\u09AF\u09BE\u09A8\u09B6\u09A8 \u09AC\u09BE `awk` \u098F\u09B0 \u09AE\
  \u09A4 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u099F\u09C1\u09B2\u09B8\u2026"
lastmod: '2024-03-17T18:47:44.206051-06:00'
model: gpt-4-0125-preview
summary: "Bash \u098F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\
  \u09BF\u0995\u09C7 \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\
  \u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\
  \u09C1 \u0986\u09AA\u09A8\u09BF \u09AA\u09CD\u09AF\u09BE\u09B0\u09BE\u09AE\u09BF\
  \u099F\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09AF\u09BE\u09A8\u09B6\u09A8\
  \ \u09AC\u09BE `awk` \u098F\u09B0 \u09AE\u09A4 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\
  \u0995 \u099F\u09C1\u09B2\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u09B8\u09AE\u09CD\u09AA\
  \u09BE\u09A6\u09A8 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u099B\u09C1 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2 Bash \u098F \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09CD\u09AF\u09BE\
  \u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09BE\u09B0."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
Bash এ স্ট্রিংগুলিকে ক্যাপিটালাইজ করার জন্য সরাসরি কোনো বিল্ট-ইন ফাংশন নেই, কিন্তু আপনি প্যারামিটার এক্সপ্যানশন বা `awk` এর মত বাহ্যিক টুলস ব্যবহার করে এই কাজটি সম্পাদন করতে পারেন। এখানে কিছু উপায় দেওয়া হল Bash এ একটি স্ট্রিং ক্যাপিটালাইজ করার:

**প্যারামিটার এক্সপ্যানশন ব্যবহার করে:**

এই পদ্ধতিটি শেলে সরাসরি স্ট্রিংটিকে ম্যানিপুলেট করে।

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
আউটপুট:
```
Hello world
```

**`awk` ব্যবহার করে:**

`awk` হলো সর্বাধিক ইউনিক্স-লাইক অপারেটিং সিস্টেম উপস্থিত একটি শক্তিশালী টেক্সট প্রসেসিং টুল, যা স্ট্রিংগুলিকে ক্যাপিটালাইজ করতে ব্যবহার করা যেতে পারে।

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
আউটপুট:
```
Hello world
```

**`sed` ব্যবহার করে:**

একটি আরো ঐতিহ্যবাহী পদ্ধতি হিসেবে, `sed` একটি স্ট্রিং-এর প্রথম অক্ষরটিকে ক্যাপিটালাইজ করতে ব্যবহৃত হতে পারে। তবে, এটি পূর্ববর্তী পদ্ধতিগুলোর তুলনায় একটু জটিল।

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
আউটপুট:
```
Hello world
```

এই স্নিপেটগুলি Bash এ একটি স্ট্রিং-এর প্রথম অক্ষরটিকে ক্যাপিটালাইজ করার উপায় দেখায়, যা টেক্সট ম্যানিপুলেট করার সময় শেল স্ক্রিপ্টিং-এর নমনীয়তা হাইলাইট করে।
