---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:38.112404-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \u09C7, \u0986\u09AA\u09A8\u09BF `datetime` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\
  \u09CB \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\u09BE\u09AC\u09C7."
lastmod: '2024-03-17T18:47:43.587812-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7, \u0986\u09AA\u09A8\u09BF `datetime`\
  \ \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
পাইথনে, আপনি `datetime` মডিউল ব্যবহার করে তারিখ তুলনা করতে পারেন। এখানে দেখানো হলো কিভাবে:

```Python
from datetime import datetime

# দুইটি তারিখ নির্ধারণ করুন
date_1 = datetime(2023, 3, 25)
date_2 = datetime(2023, 4, 1)

# তারিখ তুলনা করুন
print(date_1 < date_2)    # আউটপুট: True
print(date_1 > date_2)    # আউটপুট: False
print(date_1 == date_2)   # আউটপুট: False

# পার্থক্য গণনা করুন
difference = date_2 - date_1
print(difference.days)    # আউটপুট: ৭
```

## গভীরে যান
তারিখের তুলনা নতুন কিছু নয়। এটি ক্যালেন্ডারের মতো পুরানো সিস্টেমে একটি প্রধান বিষয় ছিল। পাইথনের `datetime` কেবল ডিজিটালি সেই ঐতিহ্য অব্যাহত রাখে। তারিখ তুলনার অন্যান্য উপায়ও আছে যেমন ইউনিক্স টাইমস্ট্যাম্প ব্যবহার করা, বা জটিল কাজের জন্য `dateutil` মতো লাইব্রেরিগুলি ব্যবহার করা। তবে `datetime` আপনার মূল সরঞ্জাম। এটি তারিখগুলিকে অবজেক্ট হিসেবে উপস্থাপন করে, তুলনা অপারেটরগুলি (`<`, `>`, `==` ইত্যাদি) ব্যবহার করে সরাসরি তুলনা করার অনুমতি দেয়। যখন আপনি তারিখগুলি বাদ দেন, তখন আপনি একটি `timedelta` অবজেক্ট পান, যা দিন, সেকেন্ড, এবং মাইক্রোসেকেন্ডে পার্থক্য জানায়।

তাছাড়া, সময় অঞ্চলগুলি আপনাকে বিভ্রান্ত করতে পারে। যদি আপনি সময় অঞ্চল জুড়ে তারিখ নিয়ে কাজ করেন, তবে আপনাকে তাদের সচেতন করতে হবে। পাইথন `pytz` লাইব্রেরি অফার করে, যা `datetime` এর সাথে ব্যবহার করে কার্যকরভাবে সময় অঞ্চল ম্যানেজ করতে পারে।

## দেখুন এছাড়াও:
- পাইথন `datetime` মডিউল ডকুমেন্টেশন: [docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- সময় অঞ্চল ম্যানেজমেন্টের জন্য: [pytz](https://pypi.org/project/pytz/)
- জটিল তারিখ ম্যানিপুলেশনের জন্য `dateutil` লাইব্রেরি: [dateutil](https://pypi.org/project/python-dateutil/)
- ইউনিক্স টাইমস্ট্যাম্পস বোঝা: [Unix Time - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)
