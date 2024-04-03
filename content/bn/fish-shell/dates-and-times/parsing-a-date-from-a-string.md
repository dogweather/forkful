---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:10.454960-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish Shell \u098F, \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AC\u09BF\u09B6\u09C7\u09B7\u09AD\u09BE\u09AC\u09C7 \u09A8\u09BF\u09B0\
  \u09CD\u09AE\u09BF\u09A4 \u0995\u09CB\u09A8\u09CB \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09A8\u09C7\u0987\u0964 \u09AC\
  \u09B0\u0982, \u09B2\u09BF\u09A8\u09BE\u0995\u09CD\u09B8 \u098F\u09AC\u0982 macOS\
  \ \u098F \u0989\u09AA\u09B2\u09AD\u09CD\u09AF `date` (\u09AF\u09C7\u09AE\u09A8 \u09AC\
  \u09BE\u09B9\u09CD\u09AF\u09BF\u0995\u2026"
lastmod: '2024-03-17T18:47:44.507866-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u098F, \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\
  \u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B6\u09C7\u09B7\u09AD\
  \u09BE\u09AC\u09C7 \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u0995\u09CB\u09A8\
  \u09CB \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u0995\u09AE\u09BE\u09A8\u09CD\
  \u09A1 \u09A8\u09C7\u0987\u0964 \u09AC\u09B0\u0982, \u09B2\u09BF\u09A8\u09BE\u0995\
  \u09CD\u09B8 \u098F\u09AC\u0982 macOS \u098F \u0989\u09AA\u09B2\u09AD\u09CD\u09AF\
  \ `date` (\u09AF\u09C7\u09AE\u09A8 \u09AC\u09BE\u09B9\u09CD\u09AF\u09BF\u0995 \u0987\
  \u0989\u099F\u09BF\u09B2\u09BF\u099F\u09BF) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u0985\u09A5\u09AC\u09BE \u099C\u099F\
  \u09BF\u09B2 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\u09AF\u09BC\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF `GNU date` \u098F\u09B0 \u09AE\u09A4 \u099C\u09A8\u09AA\
  \u09CD\u09B0\u09BF\u09AF\u09BC \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\
  \u09CD\u099F\u09BF \u099F\u09C1\u09B2\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09BE\
  \u09B9\u09BE\u09AF\u09CD\u09AF \u09A8\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09AF\
  \u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7\
  \ \u098F\u0997\u09BF\u09DF\u09C7 \u09AF\u09C7\u09A4\u09C7 \u09B9\u09AC\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
Fish Shell এ, স্ট্রিং থেকে তারিখ পার্স করার জন্য বিশেষভাবে নির্মিত কোনো বিল্ট-ইন কমান্ড নেই। বরং, লিনাক্স এবং macOS এ উপলভ্য `date` (যেমন বাহ্যিক ইউটিলিটি) ব্যবহার করা হয় অথবা জটিল পার্সিংয়ের জন্য `GNU date` এর মত জনপ্রিয় থার্ড-পার্টি টুলগুলির সাহায্য নেওয়া হয়। এখানে কিভাবে এগিয়ে যেতে হবে:

**Fish এর সাথে `date` ব্যবহার করা:**

"YYYY-MM-DD" ফর্ম্যাটে একটি তারিখ স্ট্রিং পার্স করতে, আপনি স্ট্রিংযুক্ত এর পরে `-d` (অথবা GNU date এর জন্য `--date`) বিকল্পটি এবং `+` বিকল্পটি ব্যবহার করতে পারেন যা আউটপুট ফর্ম্যাট করে।

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# আউটপুট: শনিবার, ০১ এপ্রিল ২০২৩
```

macOS এর জন্য (যা `-j` এবং `-f` ফ্ল্যাগের জন্য আলাদা ফর্ম্যাটের প্রয়োজন):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# আউটপুট: শনিবার, ০১ এপ্রিল ২০২৩
```

**জটিল পার্সিংয়ের জন্য GNU `date` ব্যবহার করা:** 

GNU `date` স্ট্রিং ফর্ম্যাটগুলির সাথে অনেক বেশি নমনীয়। এটি অনেক সাধারণ তারিখ স্ট্রিং ফর্ম্যাট স্বয়ংক্রিয়ভাবে চিহ্নিত করতে পারে ইনপুট ফর্ম্যাটটি সুস্পষ্ট ভাবে নিদিষ্ট করা ছাড়া:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# আউটপুট: ২০২৩-০৪-০১ ১৪:০০:০০
```

তবে, যেসব তারিখ স্ট্রিংগুলি স্বয়ংক্রিয়ভাবে চিহ্নিত না হতে পারে অথবা যখন ইনপুট ফর্ম্যাটের ওপর নির্দিষ্ট নিয়ন্ত্রণ প্রয়োজন, সেই ক্ষেত্রে GNU `date` এর সাথে ইনপুট ফর্ম্যাট সরাসরি নির্দিষ্ট করা সমর্থিত নয়। এ ধরণের পরিস্থিতিতে, স্ট্রিংটি প্রিপ্রসেসিং করা অথবা আরও জটিল তারিখ পার্সিং রুটিনের জন্�
