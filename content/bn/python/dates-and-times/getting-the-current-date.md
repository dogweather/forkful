---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:38.257337-06:00
description: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\
  \u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u0986\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0985\u09A8\u09C7\u0995 \u0985\u09CD\
  \u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0995\u09BE\
  \u099C, \u09AF\u09C7\u09AE\u09A8 \u09B2\u0997\u09BF\u0982, \u09A1\u09C7\u099F\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3, \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09AF\u09BC-\u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B8\u09BF\u09A6\u09CD\
  \u09A7\u09BE\u09A8\u09CD\u09A4 \u0997\u09CD\u09B0\u09B9\u09A3\u0964 \u098F\u099F\
  \u09BF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.585734-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\
  \u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u0986\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE \u0985\u09A8\u09C7\u0995 \u0985\u09CD\
  \u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\u0995 \u0995\u09BE\
  \u099C, \u09AF\u09C7\u09AE\u09A8 \u09B2\u0997\u09BF\u0982, \u09A1\u09C7\u099F\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3, \u098F\u09AC\u0982 \u09B8\u09AE\
  \u09AF\u09BC-\u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0995 \u09B8\u09BF\u09A6\u09CD\
  \u09A7\u09BE\u09A8\u09CD\u09A4 \u0997\u09CD\u09B0\u09B9\u09A3\u0964 \u098F\u099F\
  \u09BF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09AC\u09B0\u09CD\
  \u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u0986\u09A8\u09BE\u09B0\
  \ \u0995\u09A5\u09BE \u09AC\u09B2\u09C7, \u09AF\u09BE \u09B8\u09AE\u09AF\u09BC \u09B8\
  \u09BE\u09AA\u09C7\u0995\u09CD\u09B7\u09C7 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\u09AF\
  \u09CB\u0997\u09CD\u09AF \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0985\u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কিভাবে:
**স্ট্যান্ডার্ড লাইব্রেরি `datetime` ব্যবহার করে:**

পাইথনের স্ট্যান্ডার্ড লাইব্রেরির `datetime` মডিউল তারিখ এবং সময় ম্যানিপুলেশনের জন্য ক্লাস প্রদান করে। বর্তমান তারিখ পেতে, আপনি `date.today()` মেথড ব্যবহার করতে পারেন।

```python
from datetime import date

today = date.today()
print(today)  # আউটপুট: YYYY-MM-DD (উদাহরণস্বরূপ, 2023-04-05)
```

**সময় ফরম্যাটিং:**

যদি আপনি বর্তমান তারিখটি ভিন্ন ফর্ম্যাটে চান, তবে `strftime` মেথড আপনাকে কাস্টম ডেট ফর্ম্যাটিং নির্দিষ্ট করা অনুমতি দেয়:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # উদাহরণ ফরম্যাট: "April 05, 2023"
print(formatted_date)
```

**আরও নমনীয়তা পেতে `pendulum` ব্যবহার করা(একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি):**

`Pendulum` একটি থার্ড-পার্টি লাইব্রেরি যা পাইথনে তারিখ ও সময় নিয়ে কাজ করার আরও সহজ এবং সহজাত উপায় প্রদান করে। এটি স্ট্যান্ডার্ড datetime ফাংশনালিটিজকে প্রসারিত করে এবং সময় অঞ্চল ম্যানেজমেন্টের মতো বিষয়গুলি সহজ করে, এর মধ্যে অন্যান্য বৈশিষ্ট্য রয়েছে।

প্রথমে, নিশ্চিত করুন আপনি `pendulum` ইনস্টল করেছেন পিপ এর মাধ্যমে:

```shell
pip install pendulum
```

তারপরে, বর্তমান তারিখ পেতে:

```python
import pendulum

today = pendulum.now().date()
print(today)  # আউটপুট: YYYY-MM-DD (উদাহরণস্বরূপ, 2023-04-05)
```

`Pendulum` এর সাথে, ফরম্যাটিংও সরল এবং `strftime` পদ্ধতির মতোই:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # ডিফল্ট ফরম্যাট: "Apr 5, 2023"
print(formatted_date)
```
