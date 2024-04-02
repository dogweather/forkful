---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:44.223591-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B8\u09B9 \u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\
  \u099F\u09BF\u0995\u09C7\u09B6\u09A8 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u0987\u0989\u099C\u09BE\u09B0\u09A8\u09C7\u09AE \u0993 \u09AA\u09BE\u09B8\u0993\
  \u09AF\u09BC\u09BE\u09B0\u09CD\u09A1\u0995\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7 \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\u09B0\u09AE\u09BE\
  \u09A3\u09BF\u09A4 \u0995\u09B0\u09BE \u09AF\u09C7 \u0986\u09AA\u09A8\u09BF \u09AA\
  \u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09A4\u09C7 \u0985\u09A8\u09C1\u09AE\
  \u09A4\u09BF \u09AA\u09C7\u09AF\u09BC\u09C7\u099B\u09C7\u09A8\u0964\u2026"
lastmod: '2024-03-17T18:47:43.572152-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B8\u09B9 \u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\
  \u099F\u09BF\u0995\u09C7\u09B6\u09A8 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u0987\u0989\u099C\u09BE\u09B0\u09A8\u09C7\u09AE \u0993 \u09AA\u09BE\u09B8\u0993\
  \u09AF\u09BC\u09BE\u09B0\u09CD\u09A1\u0995\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\u09C7 \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\u09C7\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\u09B0\u09AE\u09BE\
  \u09A3\u09BF\u09A4 \u0995\u09B0\u09BE \u09AF\u09C7 \u0986\u09AA\u09A8\u09BF \u09AA\
  \u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09A4\u09C7 \u0985\u09A8\u09C1\u09AE\
  \u09A4\u09BF \u09AA\u09C7\u09AF\u09BC\u09C7\u099B\u09C7\u09A8\u0964\u2026"
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কি এবং কেন?

HTTP অনুরোধ পাঠানো সহ বেসিক অথেন্টিকেশন হল আপনার ইউজারনেম ও পাসওয়ার্ডকে একটি সার্ভারে অনুরোধের মাধ্যমে প্রমাণিত করা যে আপনি প্রবেশ করতে অনুমতি পেয়েছেন। প্রোগ্রামাররা এটি করে থাকেন API অথবা ওয়েব সার্ভিসের সাথে ইন্টার‍্যাক্ট করার জন্য যা ইউজার-ক্রিডেনশিয়ালস গেটের পিছনে লক করা থাকে।

## কিভাবে:

এখানে দেখানো হল কিভাবে Python ব্যবহার করে একটি সার্ভারের সাথে বেসিক অথ ব্যবহার করে কথা বলবেন।

```Python
import requests
from requests.auth import HTTPBasicAuth

# আপনার আসল ক্রিডেনশিয়ালস এবং যে API এন্ডপয়েন্টে আপনি হিট করছেন সেটি প্রতিস্থাপন করুন
username = 'cooluser'
password = 'supersecretpassword'
url = 'https://api.someservice.com/data'

response = requests.get(url, auth=HTTPBasicAuth(username, password))

# চেক করুন আমরা কি পেলাম ফিরে
print(response.status_code)
print(response.json())  # ধরে নেওয়া হয়েছে রেস্পন্স JSON ফরম্যাটে আছে
```

যদি সব কিছু সমতল হয়ে যায়:

```
200
{'data': 'আপনার সিক্রেট জিনিস!'}
```

কিন্তু যদি আপনি ক্রিডেনশিয়ালসে ভুল করে থাকেন:

```
401
```

সেখানে কোন প্রবেশের চিহ্ন নেই।

## গভীরে ডুব দিন

ইতিহাসের দিক থেকে দেখলে, HTTP বেসিক অথ ওয়েব নিরাপত্তার জন্য পুরনো স্কুলের মধ্যে পড়ে, যা একটি সাইটের সাথে গোপন হাত মিলানোর এক সাধারণ উপায়। এটি নিজে নিজে খুব নিরাপদ নয় কারণ এটি ক্রিডেনশিয়ালগুলোকে প্লেইন টেক্সটে পাঠায়, কেবল বেস64 এনকোডেড – এনক্রিপ্টেড নয়। সর্বদা HTTPS ব্যবহার করুন যাতে ক্রিডেনশিয়ালগুলোকে আর বাচ্চা থেকে ক্যান্ডি ছিনতাই করার মত সহজ হয়ে না যায়।

আরও নিরাপদ বিকল্প আছে, যেমন Digest Access Authentication যেখানে পাসওয়ার্ডটি কখনো নেটওয়ার্কের ওপর দিয়ে প্লেইনে পাঠানো হয় না। OAuth আরেকটি, বিশেষ করে আজকের API গুলির জন্য। এটি প্রতিবার ID দেখানোর চেয়ে একটি অস্থায়ী VIP পাস ইস্যু করার মতো।

অন্তরালে, `requests` লাইব্রেরি আপনার ইউজারনেম এবং পাসওয়ার্ডকে এনকোড করে এবং তাদেরকে একটি `Authorization` হেডারে ফরম্যাট করে `Basic base64encodedcredentials` হিসেবে রেখে দেয়। সার্ভার এই হেডারটি ডিকোড করে, আপনার ক্রেডস চেক করে এবং যদি আপনি বৈধ ব্যবহারকারী হন, তবে আপনাকে প্রবেশের অনুমতি দেয়।

## আরো দেখুন

- অফিসিয়াল `requests` লাইব্রেরির ডক্সে আপনি অথেন্টিকেশন ও আরো অনেক কিছু সম্পর্কে জানতে পারবেন: https://docs.python-requests.org/en/latest/
- `http.client` যারা তৃতীয় পক্ষের লাইব্রেরি ছাড়া থাকতে চান: https://docs.python.org/3/library/http.client.html
- রিয়েল পাইথন HTTP বেসিক ও পাইথন সম্পর্কে গভীরে যায়: https://realpython.com/python-requests/
