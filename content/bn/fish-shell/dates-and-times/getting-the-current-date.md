---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:45.556948-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish Shell \u09AC\u09B0\u09CD\u09A4\
  \u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 `date`\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09AC\u09BE\u0987\u09B0\u09C7\u09B0 \u0995\u09AE\
  \u09BE\u09A8\u09CD\u09A1\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7, \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0985\
  \u09A8\u09C1\u09B8\u09BE\u09B0\u09C7 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AB\
  \u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0982\u09DF\u09C7\u09B0 \u09A6\u09BF\
  \u0995 \u09A5\u09C7\u0995\u09C7 \u09A8\u09AE\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u099F\u09BF\
  \u2026"
lastmod: '2024-03-17T18:47:44.508941-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 `date` \u098F\u09B0 \u09AE\u09A4\u09CB \u09AC\
  \u09BE\u0987\u09B0\u09C7\u09B0 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1\u0997\u09C1\u09B2\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0985\u09A8\u09C1\u09B8\u09BE\u09B0\u09C7\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\
  \u099F\u09BF\u0982\u09DF\u09C7\u09B0 \u09A6\u09BF\u0995 \u09A5\u09C7\u0995\u09C7\
  \ \u09A8\u09AE\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09DF \u098F\u0996\u09BE\
  \u09A8\u09C7."
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
