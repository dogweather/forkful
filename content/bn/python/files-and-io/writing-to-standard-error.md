---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:12.422521-06:00
description: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0-\u098F \u09B2\u09C7\
  \u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u098F\u09B0\
  \u09B0 \u09AE\u09C7\u09B8\u09C7\u099C \u09AC\u09BE \u09A1\u09BE\u09AF\u09BC\u09BE\
  \u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u098F\u09B0\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 (`stderr`),\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F (`stdout`) \u09A5\u09C7\u0995\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.592312-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0-\u098F \u09B2\u09C7\
  \u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u098F\u09B0\
  \u09B0 \u09AE\u09C7\u09B8\u09C7\u099C \u09AC\u09BE \u09A1\u09BE\u09AF\u09BC\u09BE\
  \u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u098F\u09B0\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 (`stderr`),\
  \ \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F (`stdout`) \u09A5\u09C7\u0995\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি এবং কেন?
পাইথনে স্ট্যান্ডার্ড এরর-এ লেখা মানে হল আপনার প্রোগ্রামের এরর মেসেজ বা ডায়াগনস্টিকসগুলিকে এরর স্ট্রিমে (`stderr`), স্ট্যান্ডার্ড আউটপুট (`stdout`) থেকে পৃথক করে নির্দেশ করা। প্রোগ্রামারগণ এটি করেন সাধারণ প্রোগ্রাম আউটপুট এবং এরর মেসেজগুলির মধ্যে পার্থক্য তৈরি করতে, যা ডিবাগিং এবং লগ বিশ্লেষণ সহজ করে তোলে।

## কিভাবে:
### `sys.stderr` ব্যবহার করে
পাইথনের অন্তর্বুদ্ধ `sys` মডিউলটি `stderr`-এ সুনির্দিষ্ট লেখা সম্ভব করে। সাধারণ এরর মেসেজ বা ডায়াগনস্টিকগুলির জন্য এই পদ্ধতিটি সোজা সাপ্টা।

```python
import sys

sys.stderr.write('Error: Something went wrong.\n')
```
স্যাম্পল আউটপুট (stderr-এ):
```
Error: Something went wrong.
```

### `print` ফাংশন ব্যবহার করে
পাইথনের `print` ফাংশন `file` প্যারামিটার নির্দিষ্ট করে `stderr`-এ তার আউটপুট রিডিরেক্ট করতে পারে। এরর মেসেজগুলি সম্ভালার জন্য `print`-এর ব্যবহারকারী-বান্ধবতাকে কাজে লাগানোর জন্য এই পদ্ধতিটি উপকারী।
```python
from sys import stderr

print('Error: Failure in module.', file=stderr)
```
স্যাম্পল আউটপুট (stderr-এ):
```
Error: Failure in module.
```

### `logging` মডিউল ব্যবহার করে
আরও সম্পূর্ণ সমাধানের জন্য, পাইথনের `logging` মডিউল মেসেজগুলিকে `stderr`-এ পাঠাতে এবং আরও অনেক কিছু, যেমন একটি ফাইলে লেখা বা মেসেজ ফর্ম্যাট কাস্টমাইজ করা, পারে। বিভিন্ন ধরনের লগিং, মেসেজ ফর্ম্যাটিং, বা গন্তব্যের প্রয়োজনীয়তা সহ অ্যাপ্লিকেশনগুলির জন্য এই পদ্ধতিটি সেরা।
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: Database connection failed.')
```
স্যাম্পল আউটপুট (stderr-এ):
```
ERROR:__main__:Error: Database connection failed.
```

### থার্ড-পার্টি লাইব্রেরি: `loguru`
`loguru` একটি জনপ্রিয় থার্ড-পার্টি লাইব্রেরি যা পাইথন অ্যাপ্লিকেশনগুলিতে লগিং সহজ করে দেয়। এটি স্বয়ংক্রিয়ভাবে এররগুলিকে `stderr`-এ নির্দেশ করে, অন্যান্য বৈশিষ্ট্যের মধ্যে।

`loguru` ব্যবহার করতে, প্রথমে এটি পিপ এর মাধ্যমে ইনস্টল করুন:
```shell
pip install loguru
```

তারপর, নিম্নলিখিতভাবে আপনার পাইথন স্ক্রিপ্টে এটি অন্তর্ভুক্ত করুন:
```python
from loguru import logger

logger.error('Error: Failed to open file.')
```
স্যাম্পল আউটপুট (stderr-এ):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: Failed to open file.
```
