---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:06.112944-06:00
description: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\
  \u09C7\u099C\u09C7\u09B0 HTML \u0995\u09CB\u09A1 \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\
  \u09B7\u09A3 \u0995\u09B0\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09A4\u09A5\u09CD\u09AF \u09AC\u09BE \u0989\u09AA\u09BE\u09A6\u09BE\u09A8\
  \ \u09A8\u09BF\u09B0\u09CD\u09AF\u09BE\u09B8 \u0995\u09B0\u09BE, \u09AF\u09BE \u0993\
  \u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09C7\u09AA\u09BF\u0982\
  , \u09A1\u09BE\u099F\u09BE \u09AE\u09BE\u0987\u09A8\u09BF\u0982 \u09AC\u09BE \u0993\
  \u09AF\u09BC\u09C7\u09AC\u09B8\u09BE\u0987\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:43.570047-06:00'
model: gpt-4-0125-preview
summary: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\
  \u09C7\u099C\u09C7\u09B0 HTML \u0995\u09CB\u09A1 \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\
  \u09B7\u09A3 \u0995\u09B0\u09BE \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\
  \u099F \u09A4\u09A5\u09CD\u09AF \u09AC\u09BE \u0989\u09AA\u09BE\u09A6\u09BE\u09A8\
  \ \u09A8\u09BF\u09B0\u09CD\u09AF\u09BE\u09B8 \u0995\u09B0\u09BE, \u09AF\u09BE \u0993\
  \u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09C7\u09AA\u09BF\u0982\
  , \u09A1\u09BE\u099F\u09BE \u09AE\u09BE\u0987\u09A8\u09BF\u0982 \u09AC\u09BE \u0993\
  \u09AF\u09BC\u09C7\u09AC\u09B8\u09BE\u0987\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \u2026"
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কি এবং কেন?
HTML পার্সিং করা মানে একটি ওয়েবপেজের HTML কোড বিশ্লেষণ করা নির্দিষ্ট তথ্য বা উপাদান নির্যাস করা, যা ওয়েব স্ক্রেপিং, ডাটা মাইনিং বা ওয়েবসাইটের সাথে স্বয়ংক্রিয় ব্যবহারকারী ইন্টার‍্যাকশন চালু করার জন্য সাধারণ কাজ। প্রোগ্রামাররা এটি করে ওয়েবসাইটের সাথে প্রোগ্রামমিকভাবে ইন্টার‍্যাক্ট করার জন্য, তথ্য নির্যাস করার জন্য, কাজগুলি স্বয়ংক্রিয় করার জন্য, অথবা ওয়েব অ্যাপ্লিকেশনগুলি পরীক্ষা করার জন্য।

## কিভাবে:
Python ওয়েব স্ক্রেপিং এবং HTML পার্সিং এর জন্য BeautifulSoup এবং requests এর মতো শক্তিশালী লাইব্রেরি প্রদান করে। শুরু করার জন্য, আপনি এই লাইব্রেরিগুলি ইনস্টল করে নিতে হবে যদি আপনি এর আগে ইনস্টল না করে থাকেন:

```bash
pip install beautifulsoup4 requests
```

এখানে একটি বেসিক উদাহরণ দেওয়া হল, যা `requests` ব্যবহার করে একটি ওয়েবপেজের HTML কনটেন্ট ফেচ করে এবং `BeautifulSoup` দ্বারা তা পার্স করে:

```python
import requests
from bs4 import BeautifulSoup

# ওয়েবপেজের কনটেন্ট ফেচ করুন
URL = 'https://example.com'
page = requests.get(URL)

# HTML কনটেন্ট পার্স করুন
soup = BeautifulSoup(page.content, 'html.parser')

# ওয়েবপেজের শিরোনাম নির্যাসের একটি উদাহরণ
title = soup.find('title').text
print(f'ওয়েবপেজের শিরোনাম: {title}')
```

**নমুনা আউটপুট**:
```
ওয়েবপেজের শিরোনাম: এক্সাম্পল ডোমেইন
```

ওয়েবপেজ থেকে সমস্ত লিঙ্ক নির্যাস করার মতো আরো জটিল কোয়েরির জন্য, আপনি BeautifulSoup এর বিভিন্ন পদ্ধতি ব্যবহার করে পার্স ট্রি নেভিগেড করা এবং খুঁজে পেতে পারেন:

```python
# <a> ট্যাগের মধ্যে সমস্ত লিঙ্ক নির্যাস করুন
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**নমুনা আউটপুট**:
```
https://www.iana.org/domains/example
```

BeautifulSoup এর নমনীয়তা আপনাকে প্রয়োজনীয় ঠিক তথ্য অনুসন্ধানের জন্য আপনার অনুসন্ধান কাস্টমাইজ করতে দেয়, ওয়েব কনটেন্ট নিয়ে কাজ করা প্রোগ্রামারদের জন্য HTML পার্সিংকে একটি শক্তিশালী টুল করে তোলে।
