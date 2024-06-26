---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:49.714963-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Python-\u098F\u09B0 `sys` \u09AE\
  \u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09B9\u099C\u09C7\u0987 \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u0986\u0981\u0995\u09A1\u09BC\u09C7\
  \ \u09A7\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AC\
  \u09C7\u09B6\u09C7\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\
  \u09A8\u09CB \u09B9\u09B2\u09CB."
lastmod: '2024-03-17T18:47:43.591233-06:00'
model: gpt-4-0125-preview
summary: "Python-\u098F\u09B0 `sys` \u09AE\u09A1\u09BF\u0989\u09B2 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7, \u0986\u09AA\u09A8\u09BF \u09B8\u09B9\
  \u099C\u09C7\u0987 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8\
  \ \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\
  \u09BF \u0986\u0981\u0995\u09A1\u09BC\u09C7 \u09A7\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6\u09C7\u09B0 \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
Python-এর `sys` মডিউল ব্যবহার করে, আপনি সহজেই কমান্ড লাইন আর্গুমেন্টগুলি আঁকড়ে ধরতে পারেন। এখানে আপনার স্ক্রিপ্টে তাদের প্রবেশের উপায় দেখানো হলো:

```python
import sys

# প্রথম আর্গুমেন্ট সবসময় স্ক্রিপ্টের নাম হয়, তাই আমরা তা বাদ দিই
arguments = sys.argv[1:]

# আর্গুমেন্টগুলি নিয়ে কিছু করুন
print("আপনি প্রবেশ করেছেন:", arguments)
```

এই ভাবে আপনার স্ক্রিপ্ট চালান:

```bash
python your_script.py these are your arguments
```

নমুনা আউটপুট:

```
আপনি প্রবেশ করেছেন: ['these', 'are', 'your', 'arguments']
```

## গভীর ডুব
অনেক আগে, মানুষ কম্পিউটারের সাথে কমান্ড লাইনের মাধ্যমে যোগাযোগ করত। তাই বেশিরভাগ ভাষা, Python সহ, কমান্ড লাইন আর্গুমেন্ট পড়ার একটি উপায় রাখে। এটিই ছিল গ্রাফিকাল ইউজার ইন্টারফেস (GUI) এর আগমনের আগে স্ক্রিপ্টগুলি নিয়ন্ত্রিত হওয়ার উপায়।

Python-এর `sys.argv` হাতের কাছে থাকলেও, আরও জটিল কমান্ড-পার্সিং নাচের জন্য `argparse` রয়েছে। `argparse` হলো এমন একটি মডিউল যখন আপনার বেসিকের চেয়ে বেশি প্রয়োজন - যেমন যখন আপনার আর্গুমেন্টগুলির নাম, প্রকার, বা ডিফল্ট মানের প্রয়োজন হয়।

এখন, `sys.argv` কেবল একটি তালিকা। আপনি যা পাস করেন সবই স্ট্রিং হিসাবে যায়, সেখানে কোন জাদু নেই; যদি আপনি সংখ্যা চান, তবে নিজেই `int()` বা `float()` এর মাধ্যমে তা পরিবর্তন করুন।

## আরও দেখুন
`sys.argv` এবং `argparse` সম্পর্কে আরও জানতে Python ডকুমেন্টশন দেখুন:

- `sys.argv`: https://docs.python.org/3/library/sys.html#sys.argv
- `argparse` টিউটোরিয়াল: https://docs.python.org/3/howto/argparse.html 

এবং যদি আপনি সত্যিকারের কমান্ড লাইন ইন্টারফেসে গভীরে ডুব দিতে চান:

- Click: https://click.palletsprojects.com/en/7.x/
- docopt: http://docopt.org/ 

শুভ কোডিং!
