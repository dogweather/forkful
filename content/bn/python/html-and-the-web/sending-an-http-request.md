---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:21.560879-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09B2 \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\
  \u09A1 \u0993\u09AF\u09BC\u09C7\u09AC\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u0985\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09B8\
  \u09CD\u099F\u09C7\u09AE \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0985\
  \u09A5\u09AC\u09BE \u09B8\u09C7\u09AC\u09BE \u099A\u09BE\u09AF\u09BC\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09AF\
  \u09BC\u09C7\u09AC API \u098F\u09B0 \u09B8\u0999\u09CD\u0997\u09C7 \u09AF\u09CB\u0997\
  \u09BE\u09AF\u09CB\u0997, \u0993\u09AF\u09BC\u09C7\u09AC \u0995\u09A8\u09CD\u099F\
  \u09C7\u09A8\u09CD\u099F\u2026"
lastmod: '2024-03-17T18:47:43.569016-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09B2 \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\
  \u09A1 \u0993\u09AF\u09BC\u09C7\u09AC\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 \u0985\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B8\u09BF\u09B8\
  \u09CD\u099F\u09C7\u09AE \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u0985\
  \u09A5\u09AC\u09BE \u09B8\u09C7\u09AC\u09BE \u099A\u09BE\u09AF\u09BC\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09AF\
  \u09BC\u09C7\u09AC API \u098F\u09B0 \u09B8\u0999\u09CD\u0997\u09C7 \u09AF\u09CB\u0997\
  \u09BE\u09AF\u09CB\u0997, \u0993\u09AF\u09BC\u09C7\u09AC \u0995\u09A8\u09CD\u099F\
  \u09C7\u09A8\u09CD\u099F\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কি এবং কেন?

HTTP অনুরোধ পাঠানো হল যখন আপনার কোড ওয়েবের মাধ্যমে অন্য একটি সিস্টেম থেকে ডেটা অথবা সেবা চায়। প্রোগ্রামাররা ওয়েব API এর সঙ্গে যোগাযোগ, ওয়েব কন্টেন্ট আনা অথবা অন্যান্য সার্ভারের সঙ্গে যোগাযোগ করার জন্য এটি করে থাকেন।

## কিভাবে:

Python-এর থার্ড-পার্টি `requests` লাইব্রেরি HTTP কল করা সহজ করে দেয়। নীচে একটি সাধারণ GET অনুরোধ পাঠানোর পদ্ধতি দেওয়া হল:

```python
import requests

response = requests.get('https://api.example.com/data')
print(response.status_code)  # রেসপন্সের স্ট্যাটাস কোড প্রদর্শন করে
print(response.json())      # যদি রেসপন্সে JSON থাকে, তা Python dict হিসেবে প্রিন্ট করে
```

JSON পেলোড এবং কাস্টম হেডারসহ আরও বিস্তারিত POST অনুরোধ:

```python
import requests
import json

url = "https://api.example.com/submit"
data = {'key': 'value'}
headers = {'Content-Type': 'application/json'}

response = requests.post(url, data=json.dumps(data), headers=headers)

print(response.status_code)
print(response.json())
```

## গভীর ডুব

HTTP অনুরোধ ওয়েবের কাজ করার উপায় — এটি ৯০-এর দশকের প্রথম দিক থেকে চালু আছে। Python-এর `requests`-এর বিকল্পগুলির মধ্যে স্ট্যান্ডার্ড লাইব্রেরির `urllib` আছে, তবে এটি একটু জটিল।

HTTP অনুরোধ পাঠানো কিভাবে করতে হয় তা বোঝা মানে মেথড (GET, POST, PUT, DELETE ইত্যাদি), স্ট্যাটাস কোড (উদা., 200 OK, 404 Not Found), হেডারস, এবং বডি ডেটা সম্পর্কে জানা।

স্ট্রিমিং অথবা এসিনক্রোনাস অনুরোধের জন্য, আপনি `requests`-এর অ্যাসিনক সংস্করণ অথবা `aiohttp` প্যাকেজ অন্বেষণ করতে পারেন। এই লাইব্রেরিগুলির নীচে, Python-এর `socket` ব্যবহার করে কাঁচা নেটওয়ার্ক যোগাযোগ ঘটে।

ঐতিহাসিকভাবে, `requests` এর সাহজ্য এবং শক্তির জন্য একটি যাওয়ার পছন্দ বিবেচনা করা হয়, তবে `httpx`, একটি নতুন অ্যাসিনক-সামঞ্জস্য লাইব্রেরি, জনপ্রিয়তা পাচ্ছে।

## আরও দেখুন

- `requests` লাইব্রেরি ডকুমেন্টেশন: https://requests.readthedocs.io
- HTTP স্ট্যাটাস কোড ব্যাখ্যা: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- Python-এর `urllib` ডকুমেন্টেশন: https://docs.python.org/3/library/urllib.html
- এসিনক HTTP অনুরোধের জন্য `httpx` লাইব্রেরি: https://www.python-httpx.org
