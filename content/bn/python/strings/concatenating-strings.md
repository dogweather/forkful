---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:22.845255-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\u09A4\u09CD\u09B0\
  \u09BF\u09A4 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0997\u09C1\u09B2\
  \u09BF \u09B6\u09C7\u09B7 \u09A5\u09C7\u0995\u09C7 \u09B6\u09C7\u09B7\u09C7 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\
  \u09C1\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09B2\u09C7\u0997\u09CB\u09B0 \u09AE\u09A4\u09CB\u0964 \u0986\u09AE\u09B0\
  \u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0987 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BF; \u09AD\u09BE\u09AC\u09C1\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.563498-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u0995\u09A4\u09CD\u09B0\
  \u09BF\u09A4 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0997\u09C1\u09B2\
  \u09BF \u09B6\u09C7\u09B7 \u09A5\u09C7\u0995\u09C7 \u09B6\u09C7\u09B7\u09C7 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A8\u09A4\
  \u09C1\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09B2\u09C7\u0997\u09CB\u09B0 \u09AE\u09A4\u09CB\u0964 \u0986\u09AE\u09B0\
  \u09BE \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0987 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BF; \u09AD\u09BE\u09AC\u09C1\u09A8 \u0987\u0989\u099C\u09BE\u09B0\u09A8\u09C7\
  \u09AE, \u098F\u09B0\u09B0 \u09AE\u09C7\u09B8\u09C7\u099C, \u098F\u09AC\u0982 \u09A1\
  \u09BE\u09AF\u09BC\u09A8\u09BE\u09AE\u09BF\u0995 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\
  \u09CD\u099F \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কিভাবে:
চলুন কিছু স্ট্রিং একত্রিত করি।

```python
first_name = "Charlie"
last_name = "Brown"
full_name = first_name + " " + last_name  # ক্লাসিক একত্রিত করা স্পেস সহ
print(full_name)
```
আউটপুট: `Charlie Brown`

`join()` ব্যবহার করে শব্দের একটি তালিকা জন্য:

```python
words = ["Hello", "world!"]
sentence = " ".join(words)
print(sentence)
```
আউটপুট: `Hello world!`

F-String (পাইথন 3.6 থেকে):

```python
user = "snoopy"
action = "flying"
log_message = f"{user} is {action} his doghouse"
print(log_message)
```
আউটপুট: `snoopy is flying his doghouse`

## গভীর ডুব
প্রোগ্রামিং শুরু থেকেই একত্রিত করা একটি মৌলিক স্ট্রিং অপারেশন হয়েছে। মনে রাখবেন, পাইথন স্ট্রিংগুলিকে অপরিবর্তনীয় হিসেবে ব্যবহার করে, তাই প্রতিটি একত্রিত করা একটি নতুন স্ট্রিং তৈরি করে।

একটি সময় ছিল, যখন প্লাস (`+`) আমাদের কাছে একমাত্র উপায় ছিল। এটি একাধিক স্ট্রিংয়ের জন্য দক্ষ নয়, কারণ এটি মেমোরি ফুলে যেতে এবং ধীর পারফরম্যান্সের কারণ হতে পারে। 'জয়েন()' পদ্ধতির জন্য সময়—এটি বিশেষ করে একটি সিরিজের স্ট্রিং ফিউজ করার জন্য মেমোরি-বান্ধব।

F-স্ট্রিংগুলি, পাইথন 3.6-এ প্রবর্তিত, একটি গেম চেঞ্জার। এগুলি পঠনযোগ্য এবং দ্রুত এবং স্ট্রিং লিটারালের মধ্যে এক্সপ্রেশন মূল্যায়নের অনুমতি দেয়—`f"{variable}"। এগুলি আধুনিক পাইথনিস্টার জন্য গিয়ে উপযুক্ত, কার্যকারিতা এবং দক্ষতা মিশ্রিত করে।

## আরও দেখুন
- [পাইথন স্ট্রিং পদ্ধতি](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 498 -- সাক্ষাত্কার স্ট্রিং ইন্টারপোলেশন](https://www.python.org/dev/peps/pep-0498/)
- [পাইথন স্ট্রিং ফর্ম্যাটিং সেরা প্র্যাকটিস](https://realpython.com/python-f-strings/)
