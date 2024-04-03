---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:21.692723-06:00
description: "\u0995\u09CB\u09A8\u09CB \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7 \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\
  \u0995\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE \u09AF\u09BE\
  \ \u09B8\u09C7\u0987 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09AA\u09CD\u09B0\
  \u0995\u09BE\u09B6 \u0995\u09B0\u09C7\u0964 \u0986\u09AE\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C1\u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09CB\u09B0 \u09AC\u09BF\
  \u09A8\u09CD\u09AF\u09BE\u09B8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:44.239242-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u09A8\u09CB \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\
  \u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u0995\u09C7\
  \ \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE \u09AF\u09BE \u09B8\u09C7\
  \u0987 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09AA\u09CD\u09B0\u0995\u09BE\
  \u09B6 \u0995\u09B0\u09C7\u0964 \u0986\u09AE\u09B0\u09BE \u09AE\u09BE\u09A8\u09C1\
  \u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09A4\
  \u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09CB\u09B0 \u09AC\u09BF\u09A8\u09CD\u09AF\
  \u09BE\u09B8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u0985\u09A5\u09AC\
  \u09BE CSV \u09AC\u09BE JSON \u098F\u09B0 \u09AE\u09A4\u09CB \u099F\u09C7\u0995\u09CD\
  \u09B8\u099F \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09A4\u09BE\u09B0\
  \u09BF\u0996\u0997\u09C1\u09B2\u09CB \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF \u0995\u09B0\u09BF\u0964."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কি এবং কেন?
কোনো তারিখকে স্ট্রিংয়ে রূপান্তর করা মানে তাকে এমন একটি টেক্সটে পরিণত করা যা সেই তারিখকে প্রকাশ করে। আমরা মানুষের পড়ার জন্য তারিখগুলোর বিন্যাস তৈরি করতে অথবা CSV বা JSON এর মতো টেক্সট ফরম্যাটে তারিখগুলো সংরক্ষণের জন্য এটি করি।

## কিভাবে:
নিচে বাশে কোনো তারিখকে স্ট্রিংয়ে রূপান্তর করার উদাহরণ দেওয়া হল:

```Bash
# ডিফল্ট ফরম্যাটে বর্তমান তারিখ এবং সময় দেখাও
echo $(date)

# কাস্টম ফরম্যাট: YYYY-MM-DD
echo $(date '+%Y-%m-%d')

# সময়সহ দেখাও
echo $(date '+%Y-%m-%d %H:%M:%S')

# একটি বিদ্যমান তারিখ রূপান্তর করা
existing_date='2023-03-17 08:00:00'
date -d "$existing_date" '+%A, %B %d, %Y'
```
উপরের কমান্ডগুলোর জন্য উদাহরণ আউটপুট:

```
Sat Mar 25 12:04:22 PDT 2023
2023-03-25
2023-03-25 12:04:22
শুক্রবার, মার্চ 17, 2023
```

## গভীর ডাইভ
ইউনিক্স-জাতীয় সিস্টেমগুলো `date` কমান্ড ব্যবহার করে আসছে তারিখ এবং সময় নিয়ন্ত্রণের জন্য প্রাথমিক সময় থেকে। এর নমনীয়তা অনেক ধরনের ফরম্যাটের সুবিধা দেয়, `%Y` যা বছরের জন্য এবং `%d` যা দিনের জন্য ফরম্যাট স্পেসিফায়ারগুলোর মাধ্যমে।

যদি আপনি ভিন্ন টেক স্ট্যাক ব্যবহার করেন, তবে `date` কমান্ডের বিকল্পও রয়েছে। যেমন, পাইথনের ক্ষেত্রে আছে `datetime.strftime`, অন্যদিকে জাভাস্ক্রিপ্টে আছে `Date` অবজেক্ট যাতে আছে `toLocaleDateString()` এর মতো মেথডগুলো।

বাশে তারিখ রূপান্তর করার সময় মনে রাখবেন যে, `date` কমান্ডটি সিস্টেমের বর্তমান টাইমস্ট্যাম্প অথবা প্রদত্ত একটি তারিখ দিয়ে কাজ করতে পারে। সঠিক তারিখ প্রদানের জন্য টাইমজোন হ্যান্ডলিংও গুরুত্বপূর্ণ।

## আরও দেখুন
- GNU coreutils 'date': https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- উন্নত বাশ-স্ক্রিপ্টিং গাইড: https://tldp.org/LDP/abs/html/
- ডেট কমান্ডের জন্য ফরম্যাট স্পেসিফায়ার: https://man7.org/linux/man-pages/man1/date.1.html
