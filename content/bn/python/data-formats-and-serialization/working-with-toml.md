---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:23.891094-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B6\u09C1\u09B0\u09C1 \u0995\
  \u09B0\u09BE\u09B0 \u0986\u0997\u09C7, `toml` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\
  \u099C \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8 `pip install\
  \ toml` \u09A6\u09BF\u09DF\u09C7\u0964 \u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\
  \u09BF TOML \u09AB\u09BE\u0987\u09B2 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BF."
lastmod: '2024-03-17T18:47:43.599944-06:00'
model: gpt-4-0125-preview
summary: "\u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u0986\u0997\u09C7, `toml`\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u0987\u09A8\u09B8\u09CD\u099F\u09B2\
  \ \u0995\u09B0\u09C1\u09A8 `pip install toml` \u09A6\u09BF\u09DF\u09C7\u0964 \u099A\
  \u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF TOML \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BF."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
শুরু করার আগে, `toml` প্যাকেজ ইনস্টল করুন `pip install toml` দিয়ে। চলুন একটি TOML ফাইল পার্স করি:

```python
import toml

# উদাহরণ TOML কন্টেন্ট একটি স্ট্রিং আকারে
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # প্রথম শ্রেণির তারিখ

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# TOML স্ট্রিং পার্স করা
parsed_toml = toml.loads(toml_string)

# ডেটা অ্যাক্সেসিং
print(parsed_toml['owner']['name'])  # আউটপুট: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # আউটপুট: [8001, 8001, 8002]
```

## গভীর ডুব
TOML টম প্রেস্টন-ওয়ের্নার দ্বারা সৃষ্টি, যিনি GitHub এর প্রতিষ্ঠাতাদের একজন, এটি একটি ব্যবহারকারী-বান্ধব কনফিগারেশন ফাইল ফরম্যাট হিসেবে ডিজাইন করা হয়েছে। এটি একটি হ্যাশ টেবিলে এক নির্দ্বিধায় ম্যাপ করার জন্য ডিজাইন করা হয়েছে এবং মেশিন দ্বারা সহজেই পার্স করা সম্ভব।

JSON এর তুলনায়, TOML কনফিগারেশন ফাইলগুলির জন্য অধিকাংশ পঠনীয় এবং মন্তব্য সমর্থন করে। YAML, একটি অন্য বিকল্প, আরও সংকুচিত হতে পারে, কিন্তু এর ইনডেনটেশনের উপর নির্ভরতা এবং যেমন ট্যাবগুলি অনুমোদিত নয় এমন সূক্ষ্ম সমস্যা, মানুষকে বিভ্রান্ত করতে পারে। 

কার্যনির্বাহের বিস্তারিত বিবরণের বিষয়ে, TOML মানগুলি টাইপড, যা স্ট্রিং, পূর্ণসংখ্যা, ফ্লোট, বুলিয়ান, ডেটাটাইম, অ্যারে, এবং টেবিল অন্তর্ভুক্ত। সবকিছু কেস সেনসিটিভ। এছাড়াও, TOML মাল্টি-লাইন স্ট্রিংস সমর্থন করে এবং, সর্বশেষ সংস্করণে, এমনকি বিভিন্ন টাইপ করা অ্যারেগুলির জন্যও অনুমোদন করে।

Python `toml` লাইব্রেরিটি ব্যবহার করে, যা JSON এবং YAML লাইব্রেরিগুলির সাথে API এর দিক দিয়ে মিল রাখে। আপনার কাছে `toml.load` এবং `toml.loads` রয়েছে একটি ফাইল বা একটি স্ট্রিং থেকে TOML পড়ার জন্য, যথাক্রমে, এবং `toml.dump` এবং `toml.dumps` রয়েছে এটি লিখতে।

## আরও দেখুন
- স্পেক্সের জন্য অফিসিয়াল TOML GitHub রিপোজিটরি: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `toml` Python লাইব্রেরি ডকুমেন্টেশন: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- TOML এর বাস্তব জগতের উদাহরণ: Rust এর প্যাকেজ ম্যানেজার `cargo` বা Python প্যাকেজিং টূল `poetry` এর কনফিগারেশন ফাইলগুলি।
