---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:36.093098-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0987\u09A5\u09A8\
  \ \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\
  \u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF `os` \u098F\u09AC\u0982 `pathlib` \u09AE\u09A1\u09BF\
  \u0989\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B8\u09CD\
  \u09AC\u09BE\u09AD\u09BE\u09AC\u09BF\u0995 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\
  \u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0989\u09AD\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983\
  \ #."
lastmod: '2024-03-17T18:47:43.590062-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8 \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\
  \u09B0\u09BF\u09B0 \u0985\u09B8\u09CD\u09A4\u09BF\u09A4\u09CD\u09AC \u09AF\u09BE\
  \u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `os` \u098F\
  \u09AC\u0982 `pathlib` \u09AE\u09A1\u09BF\u0989\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u09B8\u09CD\u09AC\u09BE\u09AD\u09BE\u09AC\u09BF\u0995\
  \ \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\
  \u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0989\u09AD\u09AF\u09BC\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09B2\u0983\n\n#."
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
weight: 20
---

## কিভাবে:
পাইথন ডিরেক্টরির অস্তিত্ব যাচাই করার জন্য `os` এবং `pathlib` মডিউলের মাধ্যমে স্বাভাবিক উপায় প্রদান করে। এখানে উভয়ের জন্য উদাহরণ দেওয়া হলঃ

### `os` মডিউল ব্যবহার করে
```python
import os

# ডিরেক্টরি পথ উল্লেখ করুন
dir_path = "/path/to/directory"

# যাচাই করুন যে ডিরেক্টরি আছে কিনা
if os.path.isdir(dir_path):
    print(f"ডিরেক্টরি {dir_path} আছে।")
else:
    print(f"ডিরেক্টরি {dir_path} নেই।")
```

### `pathlib` মডিউল ব্যবহার করে
```python
from pathlib import Path

# ডিরেক্টরি পথ উল্লেখ করুন
dir_path = Path("/path/to/directory")

# যাচাই করুন যে ডিরেক্টরি আছে কিনা
if dir_path.is_dir():
    print(f"ডিরেক্টরি {dir_path} আছে।")
else:
    print(f"ডিরেক্টরি {dir_path} নেই।")
```

### তৃতীয়-পক্ষের লাইব্রেরি
যদিও পাইথনের স্ট্যান্ডার্ড লাইব্রেরি ডিরেক্টরি আছে কিনা যাচাই করার জন্য যথেষ্ট, `pathlib2` এর মত লাইব্রেরি পাইথন ভার্সন জুড়ে ধারাবাহিকতা বা অতিরিক্ত ফাংশনালিটির জন্য বিকল্প হতে পারে।

***মনে রাখা:*** সর্বশেষ পাইথন ভার্সনগুলি অনুসারে, `pathlib` বেশিরভাগ ব্যবহারের ক্ষেত্রে যথেষ্ট দৃঢ়, যা এই নির্দিষ্ট কাজের জন্য তৃতীয়-পক্ষের লাইব্রেরিগুলিকে কম প্রয়োজনীয় করে তোলে।
