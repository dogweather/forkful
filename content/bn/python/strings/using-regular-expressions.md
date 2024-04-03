---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:26.155410-06:00
description: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\
  \u09CD\u09B0\u09C7\u09B6\u09A8 (regex) \u09B9\u09B2 \u09AA\u09CD\u09AF\u09BE\u099F\
  \u09BE\u09B0\u09CD\u09A8\u0997\u09C1\u09B2\u09BF \u09AF\u09BE \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u09B8\u09AE\u09A8\u09CD\u09AC\u09DF \u0996\
  \u09C1\u0981\u099C\u09C7 \u09AA\u09C7\u09A4\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09BF\u09A4 \u09AA\u09CD\u09AF\u09BE\
  \u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.561483-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\
  \u09CD\u09B0\u09C7\u09B6\u09A8 (regex) \u09B9\u09B2 \u09AA\u09CD\u09AF\u09BE\u099F\
  \u09BE\u09B0\u09CD\u09A8\u0997\u09C1\u09B2\u09BF \u09AF\u09BE \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0985\
  \u0995\u09CD\u09B7\u09B0\u09C7\u09B0 \u09B8\u09AE\u09A8\u09CD\u09AC\u09DF \u0996\
  \u09C1\u0981\u099C\u09C7 \u09AA\u09C7\u09A4\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0\
  \ \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09BF\u09A4 \u09AA\u09CD\u09AF\u09BE\
  \u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u09A4\
  \u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u0996\u09CB\u0981\u099C\u09BE, \u09B8\
  \u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09BE \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8, \u09AF\u09BE \u09A1\u09C7\u099F\
  \u09BE \u09AF\u09BE\u099A\u09BE\u0987\u0995\u09B0\u09A3, \u09AA\u09BE\u09B0\u09CD\
  \u09B8\u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\
  \u09A4\u09B0\u09C7\u09B0 \u09AE\u09A4\u09CB \u0995\u09BE\u099C\u09C7 \u0985\u09AA\
  \u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF\u0964."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
পাইথনে regex ব্যবহার করা মানে `re` মডিউল ব্যবহার করা, যা আপনাকে রেগুলার এক্সপ্রেশন ব্যবহার করে টেক্সট প্রক্রিয়া করার জন্য একটি সেট ফাংশন প্রদান করে।

### বেসিক প্যাটার্ন ম্যাচিং
একটি স্ট্রিংয়ের মধ্যে একটি প্যাটার্ন খুঁজতে `re.search()` ব্যবহার করুন। যখন প্যাটার্নটি পাওয়া যায়, তখন এটি একটি ম্যাচ অবজেক্ট রিটার্ন করে, না হলে `None`।
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("প্যাটার্ন পাওয়া গেছে!")
else:
    print("প্যাটার্ন পাওয়া যায়নি।")
```
আউটপুট:
```
প্যাটার্ন পাওয়া গেছে!
```

### রেগুলার এক্সপ্রেশন কম্পাইল করা
একই প্যাটার্ন বারবার ব্যবহার করার জন্য, ভালো পারফরমান্সের জন্য প্রথমে `re.compile()` দ্বারা তা কম্পাইল করুন।
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("কম্পাইলড প্যাটার্ন পাওয়া গেছে!")
```
আউটপুট:
```
কম্পাইলড প্যাটার্ন পাওয়া গেছে!
```

### স্ট্রিং বিভাজন
একটি স্ট্রিংকে regex প্যাটার্নের প্রতিটি ম্যাচে বিভাজন করতে `re.split()` ব্যবহার করুন।
```python
result = re.split("\s", "Python is fun")
print(result)
```
আউটপুট:
```
['Python', 'is', 'fun']
```

### সব মিল খুঁজে পাওয়া
একটি প্যাটার্নের সব অনাবৃত্তিরোধিক ঘটনা খুঁজতে `re.findall()` ব্যবহার করুন।
```python
matches = re.findall("n", "Python programming")
print(matches)
```
আউটপুট:
```
['n', 'n']
```

### টেক্সট প্রতিস্থাপন
একটি প্যাটার্নের বর্তমান স্ট্রিং নতুন স্ট্রিং দিয়ে প্রতিস্থাপন করতে `re.sub()` ব্যবহার করুন।
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
আউটপুট:
```
Python is awesome
```

### থার্ড-পার্টি লাইব্রেরি
যদিও Python-এর নিজস্ব `re` মডিউল শক্তিশালী, থার্ড-পার্টি লাইব্রেরিগুলি যেমন `regex` আরও বেশি বৈশিষ্ট্য এবং উন্নত পারফরমান্স অফার করে। `regex` ব্যবহার করতে চাইলে তা পিপ (`pip install regex`) এর মাধ্যমে ইন্সটল করুন এবং আপনার কোডে তা ইম্পোর্ট করুন।

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"ভার্সন পাওয়া গেছে: {match.group(1)}")
```
আউটপুট:
```
ভার্সন পাওয়া গেছে: 3.8
```
