---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:06.112944-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Python \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09C7\u09AA\u09BF\u0982 \u098F\u09AC\u0982\
  \ HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\
  \u09AF BeautifulSoup \u098F\u09AC\u0982 requests \u098F\u09B0 \u09AE\u09A4\u09CB\
  \ \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\
  \u09C7\u0964 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, \u0986\u09AA\u09A8\u09BF \u098F\u0987\u2026"
lastmod: '2024-03-17T18:47:43.570047-06:00'
model: gpt-4-0125-preview
summary: "Python \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09C7\
  \u09AA\u09BF\u0982 \u098F\u09AC\u0982 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\
  \ \u098F\u09B0 \u099C\u09A8\u09CD\u09AF BeautifulSoup \u098F\u09AC\u0982 requests\
  \ \u098F\u09B0 \u09AE\u09A4\u09CB \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\
  \u09C0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BF \u098F\u0987 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u0987\u09A8\
  \u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C7 \u09A8\u09BF\u09A4\u09C7 \u09B9\u09AC\
  \u09C7 \u09AF\u09A6\u09BF \u0986\u09AA\u09A8\u09BF \u098F\u09B0 \u0986\u0997\u09C7\
  \ \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u09A8\u09BE \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u09A8."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

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
