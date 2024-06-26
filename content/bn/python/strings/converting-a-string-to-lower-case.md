---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:35.200693-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \ `.lower()` \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\
  \u09B9\u099C\u0964."
lastmod: '2024-03-17T18:47:43.558019-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\
  \u09CD\u09A4\u09B0 \u0995\u09B0\u09BE `.lower()` \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09B8\u09B9\u099C\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
পাইথনে একটি স্ট্রিংকে ছোট হাতের অক্ষরে রূপান্তর করা `.lower()` মেথডের সাথে সহজ।
```Python
original_string = "Hello, World!"
lowercase_string = original_string.lower()
print(lowercase_string)  # আউটপুট: hello, world!
```
অথবা আরও নিয়ন্ত্রণের জন্য লিস্ট কমপ্রিহেনশন ব্যবহার করুন:
```Python
s = "HELLO, World!"
lower_list = [char.lower() for char in s]
print(''.join(lower_list))  # আউটপুট: hello, world!
```

## গভীর ডুব
`.lower()` মেথডটি পাইথনের স্ট্রিং টাইপে বেশ প্রাথমিক সময় থেকে একটি অংশ হিসেবে রয়েছে। এটি অক্ষরভেদে অসংবেদনশীল ডেটা প্রসেসিং-এ নিশ্চিত করার একটি সরল উপায়, যা অক্ষরভেদে অসংবেদনশীল ব্যবহারকারী ইনপুটের মতো পরিস্থিতিতে উপকারী।

বিকল্প আছে, যেমন নিয়মিত এক্সপ্রেশন ব্যবহার করা:
```Python
import re

s = "HELLO, World!"
lower_s = re.sub(r'[A-Z]', lambda match: match.group(0).lower(), s)
print(lower_s)  # আউটপুট: hello, world!
```
কিন্তু একটি স্ট্রিংকে ছোট হাতের অক্ষরে পরিবর্তন করার জন্য এটি অনেক কয়েদের।

অভ্যন্তরীণ ভাবে, পাইথনের `.lower()` ইউনিকোড অক্ষর ম্যাপিং-এর উপর নির্ভর করে। ইউনিকোড স্ট্যান্ডার্ড প্রায় সমস্ত অক্ষরের ছোট হাতের সমতুল্য নির্দিষ্ট করে থাকে যাদের একটি কেস আছে। এই প্রক্রিয়া 'A' থেকে 'a'-তে যাওয়ার জন্য মাত্র একটি মান বিয়োগ করার চেয়ে বেশি জটিল, কারণ সব ভাষা এবং স্ক্রিপ্টের এমন সরল এবং সরাসরি ম্যাপিং নেই।

## আরো দেখুন
- পাইথন ডকুমেন্টেশনে স্ট্রিং মেথডস সম্পর্কে: https://docs.python.org/3/library/stdtypes.html#string-methods
- ইউনিকোড কেস ম্যাপিং বিশদ বিবরণী: https://www.unicode.org/reports/tr21/tr21-5.html
- পাইথন লিস্ট কমপ্রিহেনশন সম্পর্কে একটি টিউটোরিয়াল: https://realpython.com/list-comprehension-python/
