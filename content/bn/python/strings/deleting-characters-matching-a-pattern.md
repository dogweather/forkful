---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09AE\u09BF \u098F\u0987\
  \ \u09A7\u09B0\u09A8\u09C7\u09B0 \u0995\u09BE\u099C \u09AA\u09CD\u09B0\u09BE\u09DF\
  \ \u09B8\u09AE\u09DF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09BF \u09AF\u09C7 \u0986\
  \u09AE\u09BF \u098F\u0995\u09C7 \u098F\u0987 \u09B8\u09BF\u09AE\u09CD\u09AA\u09B2\
  \ `delete()` \u09AB\u09BE\u0982\u09B6\u09A8\u09C7 \u09B0\u09BF\u09AB\u09CD\u09AF\
  \u09BE\u0995\u09CD\u099F\u09B0 \u0995\u09B0\u09C7\u099B\u09BF\u0964 \u098F\u099F\
  \u09BF\u2026"
lastmod: '2024-04-05T21:53:51.563484-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09AE\u09BF \u098F\u0987 \u09A7\u09B0\u09A8\u09C7\u09B0 \u0995\u09BE\
  \u099C \u09AA\u09CD\u09B0\u09BE\u09DF \u09B8\u09AE\u09DF \u0995\u09B0\u09C7 \u09A5\
  \u09BE\u0995\u09BF \u09AF\u09C7 \u0986\u09AE\u09BF \u098F\u0995\u09C7 \u098F\u0987\
  \ \u09B8\u09BF\u09AE\u09CD\u09AA\u09B2 `delete()` \u09AB\u09BE\u0982\u09B6\u09A8\
  \u09C7 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\u09B0\
  \u09C7\u099B\u09BF\u0964 \u098F\u099F\u09BF [doctests](https://docs.python.org/3/library/doctest.html)\
  \ -\u098F\u09B0 \u098F\u0995\u099F\u09BF \u09AD\u09BE\u09B2\u09CB \u09A1\u09C7\u09AE\
  \u09CB\u09B8\u09CD\u099F\u09CD\u09B0\u09C7\u09B6\u09A8\u0993 \u09AC\u099F\u09C7."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09BF\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\
  \u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u09AE\u09BF\u09B2\u09C7 \u09AF\u09BE\u0993\u09AF\u09BC\u09BE \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 5
---

## কিভাবে:

```Python
import re

# উদাহরণ স্ট্রিং
text = "Hello, World! 1234"

# সব সংখ্যা মুছে ফেলুন
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # আউটপুট: "Hello, World! "

# বিরামচিহ্ন মুছে ফেলুন
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # আউটপুট: "Hello World 1234"

# স্বরবর্ণ মুছে ফেলুন
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # আউটপুট: "Hll, Wrld! 1234"
```

### আমার কাস্টম ফাংশন

আমি এই ধরনের কাজ প্রায় সময় করে থাকি যে আমি একে এই সিম্পল `delete()` ফাংশনে রিফ্যাক্টর করেছি। এটি [doctests](https://docs.python.org/3/library/doctest.html) -এর একটি ভালো ডেমোস্ট্রেশনও বটে:

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```

## গভীর অনুসন্ধান
টেক্সটে নির্দিষ্ট প্যাটার্নের অক্ষরগুলো মুছে ফেলার প্র্যাকটিসটি কম্পিউটার বিজ্ঞানে গভীর মূল নিয়েছে, যা `sed` এবং `grep` এর মতো প্রাথমিক Unix সরঞ্জামগুলো থেকে উদ্ভূত। পাইথনে, `re` মডিউলটি এই ক্ষমতাটি প্রদান করে, রেগুলার এক্সপ্রেশনস ব্যবহার করে—একটি শক্তিশালী এবং বহুমুখী টুল টেক্সট প্রসেসিংয়ের জন্য।

`re` মডিউলের বিকল্পগুলি অন্তর্ভুক্ত:
- সাধারণ কারণে `replace()` এর মতো স্ট্রিং মেথড।
- জটিল প্যাটার্ন এবং উন্নত Unicode সমর্থনের জন্য `regex` এর মতো থার্ড-পার্টি লাইব্রেরিগুলি।

যখন আপনি `re.sub()` ব্যবহার করেন, পাইথন ইন্টারপ্রিটারটি প্যাটার্নটিকে একগুচ্ছ বাইটকোডে কম্পাইল করে, যা ইনপুট টেক্সটে সরাসরি প্যাটার্ন-ম্যাচিং পারফর্ম করে এমন একটি স্টেট মেশিন দ্বারা প্রক্রিয়া করা হয়। বড় স্ট্রিং বা জটিল প্যাটার্নের জন্য এই অপারেশনটি রিসোর্স-ইনটেনসিভ হতে পারে, তাই বিগ ডাটা প্রসেসিংয়ের জন্য পারফরম্যান্স বিবেচনা অত্যন্ত জরুরি।

## আরও দেখুন
- [পাইথন `re` মডিউল ডকুমেন্টেশন](https://docs.python.org/3/library/re.html): পাইথনে রেগুলার এক্সপ্রেশনসের অফিসিয়াল ডক্স।
- [Regular-Expressions.info](https://www.regular-expressions.info/): রেগুলার এক্সপ্রেশনসের একটি সম্পূর্ণ গাইড।
- [রিয়েল পাইথন টিউটোরিয়াল অন রেজেক্স](https://realpython.com/regex-python/): পাইথনে রেগুলার এক্সপ্রেশনসের বাস্তব দুনিয়ার প্রয়োগ।
