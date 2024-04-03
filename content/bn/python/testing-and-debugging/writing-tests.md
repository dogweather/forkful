---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:41:22.874552-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \u09C7 \u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\
  \ \u09AE\u09A1\u09BF\u0989\u09B2 \u0986\u099B\u09C7 \u09AF\u09BE\u0995\u09C7 \u09AC\
  \u09B2\u09C7 `unittest`\u0964 \u098F\u09AD\u09BE\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF\
  \ \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AB\u09BE\u0982\
  \u09B6\u09A8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8."
lastmod: '2024-03-17T18:47:43.576945-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u099F\u09C7\u09B8\u09CD\u099F \u09B2\
  \u09C7\u0996\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AC\
  \u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AE\u09A1\u09BF\u0989\u09B2 \u0986\u099B\
  \u09C7 \u09AF\u09BE\u0995\u09C7 \u09AC\u09B2\u09C7 `unittest`\u0964 \u098F\u09AD\
  \u09BE\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09B0\u09C0\u0995\
  \u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

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
