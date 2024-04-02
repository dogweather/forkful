---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:29.542563-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\
  \u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF\u09C7\u09B0\
  \ \u0986\u0997\u09C7 \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\u09C7\u09B0 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B0\u09BF\u09AE\u09BE\
  \u0987\u09A8\u09CD\u09A1\u09BE\u09B0 \u09B8\u09C7\u099F \u0995\u09B0\u09BE, \u09A8\
  \u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09BF\u09A4 \u0995\u09BE\u099C \u099A\u09BE\
  \u09B2\u09BE\u09A8\u09CB, \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.241383-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\u09C7 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\
  \u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF\u09C7\u09B0\
  \ \u0986\u0997\u09C7 \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\u09C7\u09B0 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B0\u09BF\u09AE\u09BE\
  \u0987\u09A8\u09CD\u09A1\u09BE\u09B0 \u09B8\u09C7\u099F \u0995\u09B0\u09BE, \u09A8\
  \u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09BF\u09A4 \u0995\u09BE\u099C \u099A\u09BE\
  \u09B2\u09BE\u09A8\u09CB, \u0985\u09A5\u09AC\u09BE\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি এবং কেন?
ভবিষ্যতে অথবা অতীতের তারিখ হিসেব করা মানে হল নির্দিষ্ট সময়ের আগে অথবা পরের তারিখ বের করা। প্রোগ্রামাররা রিমাইন্ডার সেট করা, নির্ধারিত কাজ চালানো, অথবা মেয়াদ শেষের তারিখ হ্যান্ডল করার মতো কাজের জন্য এটি করে থাকেন।

## কিভাবে:
Bash-এ, আপনি `date` কমান্ডের সাথে `-d` ফ্লাগ ব্যবহার করে তারিখ পরিবর্তণ করতে পারেন। এখানে দেখানো হলো কিভাবে:

```Bash
# বর্তমান তারিখ
date

# ভবিষ্যত তারিখ: এখন থেকে 10 দিন পরে
date -d "+10 days"

# অতীতের তারিখ: 10 দিন আগে
date -d "-10 days"

# নির্দিষ্ট ভবিষ্যত তারিখ: সপ্তাহ, মাস, বছর যোগ করা
date -d "+1 month"
date -d "+2 weeks"
date -d "+1 year"

# ভবিষ্যত তারিখের জন্য নমুনা আউটপুট
Mon 31 Jan 2023 12:34:56 PM PST
```

## গভীরে ডুব দেওয়া
স্ক্রিপ্টিং এবং প্রোগ্রামিংয়ে তারিখ পরিবর্তন একটি সাধারণ প্রয়োজন। ঐতিহাসিকভাবে, লিপ বছর, টাইমজোন ইত্যাদি পরিচালনা করা অধিক জটিল এবং ভুলভ্রান্তিপূর্ণ ছিল। Unix-এর মতো সিস্টেমে, `date` কমান্ডে সহজ তারিখ হিসেবের জন্য অপশনগুলি যোগ করা হয়েছে।

বিকল্পগুলি মধ্যে আছে শেল অঙ্ক অথবা `awk` বা `perl` এর মতো বাহ্যিক টুল ব্যবহার করা জটিল তারিখ যুক্তির জন্য, কিন্তু মৌলিক অপারেশনের জন্য `date` কমান্ড সবচেয়ে সহজ এবং সরাসরি থাকে। অভ্যন্তরীণভাবে, `date` কমান্ড সময় গণনার জটিলতা পরিচালনা করতে সিস্টেম লাইব্রেরিগুলি ব্যবহার করে, এটি ব্যবহারকারী থেকে আবষ্ট্রাক্ট করে।

## আরও দেখুন
- GNU Coreutils ম্যানুয়াল তারিখ অধ্যায়: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- আরও উদাহরণ ও ব্যবহার ক্ষেত্র: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- উন্নত Bash-স্ক্রিপ্টিং গাইড: https://tldp.org/LDP/abs/html/abs-guide.html
