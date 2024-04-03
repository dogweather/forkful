---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:45.556948-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09C7\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0995\u09BE\u099C \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09A1\u09C7\u099F\u09BE\
  \ \u0986\u09B9\u09B0\u09A3 \u098F\u09AC\u0982 \u09A8\u09BF\u09AA\u09C1\u09A3\u09AD\
  \u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\
  \u09CD\u099F\u09BF\u0982 \u098F\u09AC\u0982 \u0985\u099F\u09CB\u09AE\u09C7\u09B6\
  \u09A8\u2026"
lastmod: '2024-03-17T18:47:44.508941-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09C7\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0995\u09BE\u099C \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\
  \u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u098F\u09AC\u0982 \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u09A1\u09C7\u099F\u09BE\
  \ \u0986\u09B9\u09B0\u09A3 \u098F\u09AC\u0982 \u09A8\u09BF\u09AA\u09C1\u09A3\u09AD\
  \u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\
  \u09CD\u099F\u09BF\u0982 \u098F\u09AC\u0982 \u0985\u099F\u09CB\u09AE\u09C7\u09B6\
  \u09A8 \u0995\u09BE\u099C\u09C7, \u099F\u09BE\u0987\u09AE\u09B8\u09CD\u099F\u09CD\
  \u09AF\u09BE\u09AE\u09CD\u09AA \u09A4\u09C8\u09B0\u09BF, \u0995\u09BE\u099C \u09A8\
  \u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u098F\u09AC\u0982 \u09B2\u0997 \u09A4\
  \u09C8\u09B0\u09BF\u09A4\u09C7 \u098F\u099F\u09BF \u0996\u09C1\u09AC\u0987 \u099C\
  \u09B0\u09C1\u09B0\u09C0\u0964."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
Fish Shell বর্তমান তারিখ পেতে `date` এর মতো বাইরের কমান্ডগুলি ব্যবহার করে, প্রয়োজন অনুসারে আউটপুট ফর্ম্যাটিংয়ের দিক থেকে নমনীয়তা প্রদান করে। এটি ব্যবহার করার উপায় এখানে:

```fish
# ডিফল্ট ফর্ম্যাটে বর্তমান তারিখ দেখাও
echo (date)

# আউটপুটের উদাহরণ: Wed 25 Oct 2023 15:42:03 BST
```

তারিখের ফরম্যাট কাস্টমাইজ করতে, আপনি ফর্ম্যাট স্পেসিফায়ারগুলির পরে `+` অপশন ব্যবহার করতে পারেন:

```fish
# YYYY-MM-DD ফর্ম্যাটে বর্তমান তারিখ দেখাও
echo (date "+%Y-%m-%d")

# আউটপুটের উদাহরণ: 2023-10-25
```

টাইমস্ট্যাম্পগুলির সাথে কাজ করা বা তারিখের অঙ্ক করা এরকম আরো জটিল কাজগুলির জন্য, Fish Shell `date` এর মতো বাইরের টুলগুলির উপর নির্ভর করে এর স্ক্রিপ্টিং স্বভাবের কারণে। এখানে বর্তমান UNIX টাইমস্ট্যাম্প পাওয়ার একটি উদাহরণ আছে:

```fish
# বর্তমান UNIX টাইমস্ট্যাম্প পেতে
echo (date "+%s")

# আউটপুটের উদাহরণ: 1666710123
```

এবং বর্তমান তারিখে একদিন যোগ করতে `date` ব্যবহার করা:

```fish
# বর্তমান তারিখে এক দিন যোগ করো
echo (date -d "+1 day" "+%Y-%m-%d")

# আউটপুটের উদাহরণ: 2023-10-26
```

মনে রাখবেন: উদাহরণগুলি GNU coreutils-এ কাজ করা `date` কমান্ডের অপশনগুলি ব্যবহার করে। অপশনগুলি macOS এর মতো অন্যান্য পরিবেশে ভিন্ন হতে পারে, যা ডিফল্টরূপে BSD date কমান্ড ব্যবহার করে। আপনার পরিবেশে বিশেষ বিবরণের জন্য সর্বদা `date --help` অথবা ম্যানুয়াল পৃষ্ঠায় রেফার করুন।
