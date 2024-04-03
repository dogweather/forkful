---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:38.561737-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09AE\u09B0\u09BE Python's\
  \ `requests` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u0964 \u09AF\u09A6\u09BF \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 \u098F\u099F\u09BF \u09A8\u09BE\
  \ \u09A5\u09BE\u0995\u09C7, \u09A4\u09BE\u09B9\u09B2\u09C7 `pip install requests`\
  \ \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\
  \u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09A6\u09CD\u09B0\u09C1\u09A4\u2026"
lastmod: '2024-03-17T18:47:43.571131-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09AE\u09B0\u09BE Python's `requests` \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09AC\u0964 \u09AF\u09A6\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\
  \u09C7 \u098F\u099F\u09BF \u09A8\u09BE \u09A5\u09BE\u0995\u09C7, \u09A4\u09BE\u09B9\
  \u09B2\u09C7 `pip install requests` \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0987\u09A8\
  \u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
আমরা Python's `requests` লাইব্রেরি ব্যবহার করব। যদি আপনার কাছে এটি না থাকে, তাহলে `pip install requests` দ্বারা ইনস্টল করুন। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```python
import requests

url = 'https://www.example.com'
response = requests.get(url)

if response.ok:
    html_content = response.text
    print(html_content)
else:
    print("Failed to retrieve the webpage")

```

এই স্ক্রিপ্টটি চালানো সফল হলে, আপনি "https://www.example.com" এর HTML কন্টেন্টগুলি আপনার কনসোলে মুদ্রিত দেখতে পাবেন।

## ডিপ ডাইভ
`requests` এর আগে, Python এ `urllib` ছিল। এটি এখনও চলমান আছে, কিন্তু `requests` এর সহজ, ব্যবহারকারী-বান্ধব ইন্টারফেস দ্বারা শো চুরি করেছে। Kenneth Reitz দ্বারা 2011 সালে প্রকাশিত `requests` থেকে, Python এ HTTP এর জন্য সোনার মানদণ্ড হয়ে উঠেছে। কিন্তু এটি শুধু সহজতার বিষয় নয় – `requests` আরও শক্তিশালী, যেমন সেশন অবজেক্ট, কুকি ধারাবাহিকতা, এবং SSL সার্টিফিকেটগুলির অটোমেটিক হ্যান্ডলিং সুবিধা প্রদান করে।

`http.client` এর মতো বিকল্প রয়েছে, যা `requests` এর তুলনায় লো-লেভেল, এবং `aiohttp` এর মতো বাহ্যিক লাইব্রেরি রয়েছে যা অ্যাসিঙ্ক অপারেশনের জন্য। যেকোন পছন্দের গভীরে, এই লাইব্রেরিগুলি ওয়েব সার্ভারের সাথে যোগাযোগ করে, HTTP অনুরোধ পাঠায়, এবং প্রতিক্রিয়া সামলায়।

পৃষ্ঠা ডাউনলোড করার সময়, `robots.txt` ফাইলগুলি সম্মান করা গুরুত্বপূর্ণ: জেনে নেওয়া যেখানে আপনি অনুমতিপ্রাপ্ত, এবং সার্ভারে বাড়তি চাপ না দিয়ে – আপনার অনুরোধগুলি ধীর করুন। এছাড়াও, মনে রাখবেন যে ওয়েব পাতাগুলি জাভাস্ক্রিপ্ট দ্বারা ডায়নামিক কন্টেন্ট টেনে আনতে পারে যা একটি সাধারণ HTTP অনুরোধের মাধ্যমে ধরা পড়বে না।

## আরও দেখুন:
- `requests` ডকুমেন্টেশন: https://requests.readthedocs.io/en/master/
- `urllib` তথ্য: https://docs.python.org/3/library/urllib.html
- `robots.txt` পরিচিতি: https://www.robotstxt.org
- `aiohttp` অ্যাসিঙ্ক ওয়েব অনুরোধের জন্য: https://docs.aiohttp.org/en/stable/
