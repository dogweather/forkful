---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:22.874552-06:00
description: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u099F\u09C7\u09B8\u09CD\u099F\
  \ \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0985\u099F\u09CB\
  \u09AE\u09C7\u099F\u09C7\u09A1 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09A0\u09BF\u0995\u09A4\u09BE \u09AD\
  \u09CD\u09AF\u09BE\u09B2\u09BF\u09A1\u09C7\u099F \u0995\u09B0\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09AF\u09BE\u09A4\u09C7\
  \ \u09A4\u09BE\u09B0\u09BE \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u09B9\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u09AF\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.576945-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u099F\u09C7\u09B8\u09CD\u099F \u09B2\
  \u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0985\u099F\u09CB\u09AE\
  \u09C7\u099F\u09C7\u09A1 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09A0\u09BF\u0995\u09A4\u09BE \u09AD\
  \u09CD\u09AF\u09BE\u09B2\u09BF\u09A1\u09C7\u099F \u0995\u09B0\u09C7\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8 \u09AF\u09BE\u09A4\u09C7\
  \ \u09A4\u09BE\u09B0\u09BE \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u09B9\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u09AF\u09C7\u2026"
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কি এবং কেন?
পাইথনে টেস্ট লেখা মানে হল অটোমেটেড স্ক্রিপ্ট তৈরি করা যা আপনার কোডের সঠিকতা ভ্যালিডেট করে। প্রোগ্রামাররা এটি করে থাকেন যাতে তারা নিশ্চিত হতে পারেন যে তাদের ফাংশন বা ক্লাসগুলো বিভিন্ন পরিস্থিতিতে প্রত্যাশিতভাবে কাজ করছে, যা ত্রুটিগুলোকে শুরুতেই ধরে ফেলার পাশাপাশি সহজে রক্ষণাবেক্ষণ এবং রিফ্যাক্টরিং সুবিধা দেয়।

## কিভাবে:
পাইথনে টেস্ট লেখার জন্য একটি বিল্ট-ইন মডিউল আছে যাকে বলে `unittest`। এভাবে আপনি এটি ব্যবহার করে একটি সাধারণ ফাংশন পরীক্ষা করতে পারেন:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Should be 12")

if __name__ == '__main__':
    unittest.main()
```

যখন আপনি এই টেস্ট স্ক্রিপ্ট চালান, আপনি দেখতে পাবেন যে আপনার টেস্টগুলো পাস করেছে (বা ব্যর্থ হয়েছে)।

আরও আধুনিক এবং এক্সপ্রেসিভ টেস্টের জন্য, আপনি `pytest` এর মত থার্ড-পার্টি লাইব্রেরি ব্যবহার করতে পারেন। প্রথমে, আপনাকে এটি pip ব্যবহার করে ইনস্টল করতে হবে:

```shell
pip install pytest
```

তারপর, আপনি কোনো কিছু সাবক্লাস না করেও আপনার টেস্টগুলো আরও সহজ উপায়ে লিখতে পারেন:

```python
# এটি একটি ফাইল হিসেবে সংরক্ষণ করুন test_with_pytest.py নামে
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Should be 12"
```

আপনার টেস্টগুলো চালাতে `pytest` ব্যবহার করে, কেবল এক্সিকিউট করুন:

```shell
pytest test_with_pytest.py
```

আপনি দেখতে পাবেন pytest থেকে আপনার টেস্টের ফলাফল দেখাচ্ছে।
