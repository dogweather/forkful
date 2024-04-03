---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:15.670653-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u098F\u0987 \u0989\
  \u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF\u09C7 `datetime` \u09AE\u09A1\u09BF\u0989\
  \u09B2 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE `strptime`\
  \ \u09AE\u09C7\u09A5\u09A1\u0995\u09C7 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7\u0964 \u098F\u0987 \u09AE\u09C7\u09A5\
  \u09A1\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A6\u09C1\u099F\u09BF \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.584564-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF \u098F\u0987 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF\
  \u09C7 `datetime` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7, \u09AF\u09BE `strptime` \u09AE\u09C7\u09A5\u09A1\u0995\u09C7\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\
  \u09C7\u0964 \u098F\u0987 \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09A6\u09C1\u099F\u09BF \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\
  \u09CD\u099F\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
পাইথনের স্ট্যান্ডার্ড লাইব্রেরি এই উদ্দেশ্যে `datetime` মডিউল প্রদান করে, যা `strptime` মেথডকে অন্তর্ভুক্ত করে। এই মেথডের জন্য দুটি আর্গুমেন্টের প্রয়োজন: তারিখের স্ট্রিং এবং একটি ফরম্যাট ডিরেক্টিভ যা ইনপুট স্ট্রিংয়ের প্যাটার্ন নির্দিষ্ট করে।

```python
from datetime import datetime

# উদাহরণ স্ট্রিং
date_string = "2023-04-01 14:30:00"
# স্ট্রিং থেকে ডেটটাইম অবজেক্টে পার্সিং
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# আউটপুট: 2023-04-01 14:30:00
```

বিশেষত, একাধিক ফর্ম্যাট বা লোকেল নিয়ে কাজ করার সময়, আরও সূক্ষ্ণ তারিখ পার্সিংয়ের জন্য থার্ড-পার্টি লাইব্রেরি `dateutil` খুবই সাহায্যকারী হতে পারে। এটি একটি পার্সার মডিউল প্রদান করে যা প্রায় যেকোনো স্ট্রিং ফরম্যাটের তারিখ পার্স করতে পারে।

```python
from dateutil import parser

# উদাহরণ স্ট্রিং
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# ডেটউটিলের পার্সার ব্যবহার করে
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# আউটপুট: 2023-04-01 14:30:00
print(parsed_date2)
# আউটপুট: 2023-04-01 14:30:00
```

`dateutil` প্রায় সব তারিখের ফর্ম্যাট সুনির্দিষ্ট ফরম্যাট স্ট্রিং ছাড়াই সামলাতে পারার ক্ষমতা থাকায়, বিভিন্ন তারিখ উপস্থাপনা নিয়ে কাজ করা অ্যাপ্লিকেশানের জন্য এটি একটি বহুমুখী পছন্দ হয়ে ওঠে।
