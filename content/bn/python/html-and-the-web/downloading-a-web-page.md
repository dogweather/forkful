---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:38.561737-06:00
description: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C3\u09B7\u09CD\u09A0\u09BE\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09C2\u09B2\
  \u09A4 \u0986\u09AA\u09A8\u09BF \u09AF\u09C7 URL \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u0995\u09B0\u09C7\u099B\u09C7\u09A8 \u09A4\u09BE \u09A5\u09C7\
  \u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\u09B9\u09B0\u09A3 \u0995\u09B0\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u09A5\u09BE\u09A8\u09C0\u09AF\u09BC\
  \ \u09AE\u09C7\u09B6\u09BF\u09A8\u09C7 \u099F\u09C7\u09A8\u09C7 \u0986\u09A8\u09BE\
  \u0995\u09C7 \u09AC\u09CB\u099D\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE,\u2026"
lastmod: '2024-03-17T18:47:43.571131-06:00'
model: gpt-4-0125-preview
summary: "\u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C3\u09B7\u09CD\u09A0\u09BE \u09A1\
  \u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u09AE\u09C2\u09B2\u09A4\
  \ \u0986\u09AA\u09A8\u09BF \u09AF\u09C7 URL \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u0995\u09B0\u09C7\u099B\u09C7\u09A8 \u09A4\u09BE \u09A5\u09C7\
  \u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0986\u09B9\u09B0\u09A3 \u0995\u09B0\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u09A5\u09BE\u09A8\u09C0\u09AF\u09BC\
  \ \u09AE\u09C7\u09B6\u09BF\u09A8\u09C7 \u099F\u09C7\u09A8\u09C7 \u0986\u09A8\u09BE\
  \u0995\u09C7 \u09AC\u09CB\u099D\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE,\u2026"
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কি এবং কেন?

ওয়েব পৃষ্ঠা ডাউনলোড করা মূলত আপনি যে URL নির্দিষ্ট করেছেন তা থেকে ডেটা আহরণ করে আপনার স্থানীয় মেশিনে টেনে আনাকে বোঝায়। প্রোগ্রামাররা ডেটা পার্স করা, পরিবর্তন নজরদারি, অথবা ওয়েবসাইটের সাথে অটোমেশন ইন্টার‍্যাকশন করার জন্য এটি করে থাকেন।

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
