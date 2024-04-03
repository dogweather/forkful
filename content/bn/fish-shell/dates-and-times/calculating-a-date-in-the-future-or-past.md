---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:44.882265-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09CE \u09AC\u09BE \u0985\u09A4\
  \u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09A8\u09BF\u09AF\u09BC\u09C7 \u09A8\u09BE\u09A8\u09BE\
  \u09A8 \u09B9\u09C7\u09B0\u09AB\u09C7\u09B0 \u0995\u09B0\u09C7 \u099C\u09BE\u09A8\
  \u09BE \u09AF\u09C7 \u0995\u09CB\u09A8 \u09A6\u09BF\u09A8\u09C7 \u09A4\u09BE \u099B\
  \u09BF\u09B2 \u09AC\u09BE \u09B9\u09AC\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09B8\u09AE\
  \u09AF\u09BC\u09B8\u09C2\u099A\u09BF, \u09AE\u09A8\u09C7 \u0995\u09B0\u09BF\u09AF\
  \u09BC\u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE,\u2026"
lastmod: '2024-03-17T18:47:44.511940-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09CE \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\
  \u09BF\u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09A8\u09BF\u09AF\u09BC\u09C7 \u09A8\u09BE\u09A8\u09BE\u09A8\
  \ \u09B9\u09C7\u09B0\u09AB\u09C7\u09B0 \u0995\u09B0\u09C7 \u099C\u09BE\u09A8\u09BE\
  \ \u09AF\u09C7 \u0995\u09CB\u09A8 \u09A6\u09BF\u09A8\u09C7 \u09A4\u09BE \u099B\u09BF\
  \u09B2 \u09AC\u09BE \u09B9\u09AC\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09B8\u09AE\u09AF\
  \u09BC\u09B8\u09C2\u099A\u09BF, \u09AE\u09A8\u09C7 \u0995\u09B0\u09BF\u09AF\u09BC\
  \u09C7 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE, \u0985\u09A5\u09AC\u09BE \u09AE\u09C7\
  \u09AF\u09BC\u09BE\u09A6 \u098F\u09AC\u0982 \u09A1\u09C7\u09A1\u09B2\u09BE\u0987\
  \u09A8 \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
এখানে ফিশ শেলে তারিখ নিয়ে চলাফেরার একটি দারুণ উপায় দেওয়া হল:

```ফিশ শেল
# বর্তমান তারিখে দিন যোগ করুন
set -l days_to_add 10
date -d "+$days_to_add days"

# আউটপুটের উদাহরণ (বর্তমান তারিখের উপর নির্ভর করে):
# Wed Mar 29 00:29:10 PDT 2023

# বর্তমান তারিখ থেকে দিন বিয়োগ করুন
set -l days_to_subtract 10
date -d "-$days_to_subtract days"

# আউটপুটের উদাহরণ (আবার, আপনার তারিখ ভিন্ন হতে পারে):
# Sun Mar 9 00:30:42 PDT 2023
```

## গভীর ডুব
ফিশ শুধুমাত্র স্প্ল্যাশ নিয়ে নয়, এর একটি ইতিহাস রয়েছে। ব্যাশের মতো শেলগুলো দিনের হিসাব করার জন্য এক সময়ের জনপ্রিয় ছিল, সাধারণত GNU `date` এর মাধ্যমে। ফিশ, এটি সহজলভ্য এবং পঠনযোগ্য রেখে, সিমান্ত বজায় রেখেছে - নতুনদের এবং অভিজ্ঞ ট্রাউটের জন্য দারুণ।

তারিখের হিসাবের বিকল্প হিসেবে পাইথনের মতো প্রোগ্রামিং ভাষা বা `dateutils` ব্যবহার করা যায়। প্রতিটির নিজস্ব শক্তি রয়েছে, যদিও `dateutils` একটু অস্পষ্ট এবং সহজ কাজের জন্য পাইথন অতিরিক্ত হতে পারে। ফিশের বাস্তবায়ন একটি মধ্যম পথ, `date` কমান্ড UNIX মানদণ্ড থেকে ধার করে নেওয়া - এটি প্রায় সর্বত্র ইন্সটল করা হয়েছে এবং সিস্টেমের সময় সেটিংসের সাথে সহজে মিশে যায়।

## আরও দেখুন
আরও বিস্তারিত জানতে এই জলে ডুব দিন:
- [GNU Coreutils – Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html): `date` কিভাবে অন্তর্নিহিত ভাবে কাজ করে তা জানুন।
- [দ্য ফিশ শেল ডকুমেন্টেশন](https://fishshell.com/docs/current/index.html): অফিসিয়াল ডক্স, যেখানে আপনি ফিশ এবং এর অন্যান্য কমান্ড সম্পর্কে জানতে পারবেন।
- [StackOverflow: তারিখ অঙ্ক](https://stackoverflow.com/questions/tagged/date-arithmetic): কমিউনিটি থেকে বাস্তব জগতের সমস্যা ও সমাধান দেখুন।
