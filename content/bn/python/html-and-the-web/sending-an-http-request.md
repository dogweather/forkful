---
title:                "HTTP অনুরোধ প্রেরণ করা"
date:                  2024-03-17T18:18:21.560879-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
