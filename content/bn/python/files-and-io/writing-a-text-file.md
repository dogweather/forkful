---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:40:38.786759-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BE\u0987\u09B2\u09C7\
  \ \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09BE\u0987\u09A5\
  \u09A8\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 `open()` \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3 \u0989\u09AA\u09BE\u09AF\u09BC\u0964 \u09AB\u09BE\u0982\u09B6\
  \u09A8\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u0996\u09CB\u09B2\u09BE\u09B0 \u09AE\
  \u09CB\u09A1 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0995\u09B0\
  \u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC - 'w'\
  \ \u0985\u09B0\u09CD\u09A5 \u09B2\u09BF\u0996\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.594548-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AA\u09BE\u0987\u09A5\u09A8\u09C7\u09B0 \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 `open()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09AC\u099A\u09C7\
  \u09AF\u09BC\u09C7 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u0989\u09AA\u09BE\u09AF\
  \u09BC\u0964 \u09AB\u09BE\u0982\u09B6\u09A8\u099F\u09BF \u09AB\u09BE\u0987\u09B2\
  \ \u0996\u09CB\u09B2\u09BE\u09B0 \u09AE\u09CB\u09A1 \u09A8\u09BF\u09B0\u09CD\u09A6\
  \u09BF\u09B7\u09CD\u099F \u0995\u09B0\u09BE\u09B0 \u0985\u09A8\u09C1\u09AE\u09A4\
  \u09BF \u09A6\u09C7\u09AF\u09BC - 'w' \u0985\u09B0\u09CD\u09A5 \u09B2\u09BF\u0996\
  \u09A8 (\u0985\u09A7\u09BF\u09B2\u09C7\u0996\u09A8), 'a' \u0985\u09B0\u09CD\u09A5\
  \ \u09AF\u09CB\u099C\u09A8, \u098F\u09AC\u0982 'w+' \u0985\u09B0\u09CD\u09A5 \u09B2\
  \u09BF\u0996\u09A8+\u09AA\u09A0\u09A8\u0964."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কিভাবে:


### বিল্ট-ইন `open()` ফাংশন ব্যবহার করে
ফাইলে লেখার জন্য পাইথনের বিল্ট-ইন `open()` ফাংশন সবচেয়ে সাধারণ উপায়। ফাংশনটি ফাইল খোলার মোড নির্দিষ্ট করার অনুমতি দেয় - 'w' অর্থ লিখন (অধিলেখন), 'a' অর্থ যোজন, এবং 'w+' অর্থ লিখন+পঠন।

```python
# একটি নতুন ফাইলে লেখা অথবা বিদ্যমান ফাইল প্রতিস্থাপন
with open('example.txt', 'w') as file:
    file.write("Hello, World!\n")

# একটি ফাইলে যোগ করা
with open('example.txt', 'a') as file:
    file.write("Appending more text.\n")

# ফাইলটি পঠনের জন্য 
with open('example.txt', 'r') as file:
    print(file.read())
```
**নমুনা আউটপুট:**
```
Hello, World!
Appending more text.
```

### `pathlib.Path` ব্যবহার করে
একটি অধিক অব্জেক্ট-ওরিয়েন্টেড দৃষ্টিকোণের জন্য, `pathlib` মডিউল থেকে `Path` ক্লাস ফাইলে লেখার একটি পদ্ধতি অফার করে। নতুন পাইথন কোডবেসগুলির জন্য এটি একটি জনপ্রিয় পদ্ধতি।

```python
from pathlib import Path

# একটি ফাইল লেখা/প্রতিস্থাপন
Path('example2.txt').write_text("This is example 2.\n")

# ফাইলটি পঠনের জন্য
print(Path('example2.txt').read_text())

# মন্তব্য: `Path.write_text` সবসময় ফাইল বিষয়বস্তুকে ওভাররাইট করে।
# যোগ করার জন্য, আপনাকে পূর্ববর্তী অনুচ্ছেদে দেখানো ভাবে ফাইলটি খুলতে হবে।
```
**নমুনা আউটপুট:**
```
This is example 2.
```

### থার্ড-পার্টি লাইব্রেরিগুলি
জটিল ফাইল অপারেশনগুলির জন্য, থার্ড-পার্টি লাইব্রেরিগুলি যেমন `pandas` (CSV, Excel ফাইলগুলির জন্য) একটি দারুণ সম্পদ হতে পারে। এখানে `pandas` ব্যবহার করে একটি DataFrame থেকে একটি CSV ফাইলে লেখার একটি দ্রুত উদাহরণ রয়েছে, যা এর ব্যবহারিকতা প্রমাণ করে কেবল সহজ টেক্সট ফাইলগুলির চেয়ে অধিক।

```python
# এর জন্য pandas প্রয়োজন: pip install pandas
import pandas as pd

# একটি সাধারণ DataFrame তৈরি
data = pd.DataFrame({'Column1': [1, 2, 3], 'Column2': ['A', 'B', 'C']})

# DataFrameটি একটি CSV ফাইলে লেখা
data.to_csv('example.csv', index=False)

# CSV টি যাচাই করার জন্য পড়া
print(pd.read_csv('example.csv'))
```
**নমুনা আউটপুট:**
```
   Column1 Column2
0        1       A
1        2       B
2        3       C
```

এই পদ্ধতিগুলি ব্যবহার করে, পাইথন প্রোগ্রামাররা ফাইল অপারেশনগুলি কার্যকরভাবে ম্যানেজ করতে পারে, সহজ এবং জটিল ডেটা হ্যান্ডলিং চাহিদার জন্য।
