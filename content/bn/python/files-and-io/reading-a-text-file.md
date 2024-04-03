---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:21.959086-06:00
description: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u09A1\u09BF\
  \u09B8\u09CD\u0995\u09C7 \u09AC\u09BE \u09A8\u09C7\u099F\u0993\u09AF\u09BC\u09BE\
  \u09B0\u09CD\u0995\u09C7\u09B0 \u0993\u09AA\u09B0\u09C7 \u0985\u09CD\u09AF\u09BE\
  \u0995\u09CD\u09B8\u09C7\u09B8\u09AF\u09CB\u0997\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u09AA\
  \u09C7\u09A4\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.593365-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\
  \u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u09B0 \u09A1\u09BF\u09B8\
  \u09CD\u0995\u09C7 \u09AC\u09BE \u09A8\u09C7\u099F\u0993\u09AF\u09BC\u09BE\u09B0\
  \u09CD\u0995\u09C7\u09B0 \u0993\u09AA\u09B0\u09C7 \u0985\u09CD\u09AF\u09BE\u0995\
  \u09CD\u09B8\u09C7\u09B8\u09AF\u09CB\u0997\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u09AA\
  \u09C7\u09A4\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\
  \u09A8\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u09A1\u09C7\u099F\
  \u09BE (\u09AF\u09C7\u09AE\u09A8 \u0995\u09A8\u09AB\u09BF\u0997\u09CD\u09AF\u09C1\
  \u09B0\u09C7\u09B6\u09A8, \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\
  \u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F, \u09B2\u0997, \u0987\u09A4\u09CD\
  \u09AF\u09BE\u09A6\u09BF) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
```python
# একবারে সমগ্র ফাইল পড়া
with open('example.txt', 'r') as file:
    content = file.read()
    print(content)

# লাইন ধরে ধরে পড়া
with open('example.txt', 'r') as file:
    for line in file:
        print(line.strip())
```

নমুনা আউটপুট:
```
এটি ফাইলের প্রথম লাইন।
এবং এটি দ্বিতীয় লাইন।
```

## গভীর ডাইভ
টেক্সট ফাইল পড়া মৌলিক - এবং প্রোগ্রামিংয়ের প্রাথমিক দিনগুলি থেকেই চালু আছে। পাইথনের সরল `open` ফাংশনের মূল সি স্ট্যান্ডার্ড লাইব্রেরি ফাংশন `fopen`-এর মধ্যে রয়েছে। টেক্সট ফাইল পড়ার জন্য কিছু বিকল্প অন্তর্ভুক্ত `pandas` লাইব্রেরি যা CSVর জন্য এবং JSON ফাইলের জন্য `json`। অভ্যন্তরীণভাবে, যখন আপনি একটি ফাইল পড়েন, পাইথন অপারেটিং সিস্টেমকে একটি ফাইল স্ট্রিম খুলতে অনুরোধ করে, যা আপনার প্রোগ্রামে ফাইল থেকে ডেটা বিলি করার মতো একটি কনভেয়ার বেল্টের মতো।

বড় ফাইলের ক্ষেত্রে, সবকিছু মেমরিতে লোড করার জন্য `read()` এর পরিবর্তে, একটি লাইন একটি সময়ে হ্যান্ডেল করার জন্য `readline()` ব্যবহার করুন অথবা ফাইল অবজেক্টকে একটি `for` লুপ দিয়ে ইতারেট করুন - এটি কার্যকর এবং মেমরি-বান্ধব। যদিও `with open` হল আধুনিক পদ্ধতি যা স্বয়ংক্রিয়ভাবে ফাইলগুলিকে বন্ধ করে, পুরানো স্ক্রিপ্টগুলি এটি ম্যানুয়ালি করার জন্য `file.close()` ব্যবহার করতে পারে, যদিও এটি ত্রুটি-ঝুঁকিপূর্ণ যদি ক্লোজ কলের আগে ব্যতিক্রম ঘটে।

## আরও দেখুন
- পাইথন ডকুমেন্টেশন IO এর উপর: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- ফাইল সম্পর্কে রিয়েল পাইথন টিউটোরিয়াল: https://realpython.com/read-write-files-python/
- `open` এর জন্য পাইথন অফিসিয়াল ডক্‌স: https://docs.python.org/3/library/functions.html#open
