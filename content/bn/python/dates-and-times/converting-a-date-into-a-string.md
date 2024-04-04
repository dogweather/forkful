---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u09AF\u09C7\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \ \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09A4\u09C7 \u09B8\u09B9\u099C \u0995\
  \u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 [\u09A6\u09BF\u09A8\u09BE\u0982\u0995\
  ](https://docs.python.org/3/library/datetime.html#date-objects) \u0985\u09AC\u099C\
  \u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\u2026"
lastmod: '2024-04-04T02:03:20.458099-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09A4\
  \u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 [\u09A6\
  \u09BF\u09A8\u09BE\u0982\u0995](https://docs.python.org/3/library/datetime.html#date-objects)\
  \ \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \ \u0989\u09AA\u09B2\u09AC\u09CD\u09A7 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\
  \u09BF\u09AD\u09BE\u09AC\u09C7."
title: "\u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\
  \u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## যেভাবে:

পাইথন আপনাকে তারিখকে স্ট্রিংয়ে রূপান্তর করতে সহজ করে তোলে। [দিনাংক](https://docs.python.org/3/library/datetime.html#date-objects) অবজেক্টগুলিতে উপলব্ধ [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) পদ্ধতিটি ব্যবহার করুন। এখানে কিভাবে:

```Python
from datetime import datetime

# বর্তমান তারিখ এবং সময় পান
now = datetime.now()

# এটিকে স্ট্রিংয়ে রূপান্তর করুন এই ফর্ম্যাটে: মাস দিন, বছর
date_string = now.strftime("%B %d, %Y")
print(date_string)  # আউটপুট: মার্চ 29, 2023 (অথবা বর্তমান তারিখ)

# ফর্ম্যাট: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # আউটপুট: 2023-03-29 (অথবা বর্তমান তারিখ)
```


### আমি যেভাবে এটি করি

[ISO 8601](https://www.w3.org/QA/Tips/iso-date) ফর্ম্যাট তারিখ সহ সময়অঞ্চলের তথ্যের সাথে এটি কিভাবে পান:

```python
def datestamp() -> str:
    """ 
    সময়অঞ্চলের তথ্য সহ আইএসও ফর্ম্যাটে বর্তমান তারিখ এবং সময়।
    """
    return datetime.now().astimezone().isoformat()
```

#### উদাহরণ আউটপুট:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## গভীর ডুব

ঐতিহাসিকভাবে, তারিখ-স্ট্রিং রূপান্তর প্রোগ্রামিংয়ে একটি মৌলিক বিষয় হয়ে ওঠে, কারণ তারিখগুলিকে মানুষের পড়ার উপযুক্ত বিন্যাসে উপস্থাপন করার প্রয়োজন ছিল।

`strftime` এর বিকল্পগুলি আইএসও ৮৬০১ বিন্যাসের জন্য `isoformat` পদ্ধতি ব্যবহার করা, বা `arrow` এবং `dateutil` এর মতো তৃতীয়-পক্ষীয় লাইব্রেরিগুলি যা আরো নমনীয় পার্সিং এবং বিন্যাস বিকল্পগুলি অফার করে।

কার্যনির্বাহণে, `strftime` মানে "স্ট্রিং ফরম্যাট সময়" এবং সি প্রোগ্রামিংয়ে এর উত্স রয়েছে। পাইথনের `strftime` বছরের জন্য `%Y` এবং মাসের জন্য `%m` এর মতো বিন্যাস কোডগুলি ব্যাখ্যা করে, যা প্রায় অসীম কাস্টমাইজেশন অনুমতি দেয়। 

## দেখুন এছাড়াও
পাইথনের তারিখ এবং সময় ফাংশনগুলিতে আরও গভীরে যেতে:
- পাইথনের অফিসিয়াল `datetime` ডকুমেন্টেশন: https://docs.python.org/3/library/datetime.html
- `strftime` নির্দেশিকাগুলির একটি ব্যাপক তালিকার জন্য: https://strftime.org/
- তৃতীয়-পক্ষীয় তারিখ/সময় লাইব্রেরিগুলিতে অন্বেষণ করতে:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
