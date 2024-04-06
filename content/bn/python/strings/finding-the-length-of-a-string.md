---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:03.474399-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0990\u09A4\u09BF\u09B9\u09BE\
  \u09B8\u09BF\u0995\u09AD\u09BE\u09AC\u09C7, `len()` \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u0996\u09CB\u0981\u099C\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AA\u09BE\u0987\u09A5\u09A8\u09C7\u09B0 \u09AA\u09CD\u09B0\u09BF\u09DF\
  \ \u0989\u09AA\u09BE\u09DF\u0964 \u098F\u099F\u09BF \u09AE\u09BE\u09B0\u09CD\u099C\
  \u09BF\u09A4 \u098F\u09AC\u0982 \u09A6\u09CD\u09B0\u09C1\u09A4\u0964 \u09AA\u09BE\
  \u0987\u09A5\u09A8\u09C7\u09B0 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\
  \u09B2\u09CB \u0987\u0989\u09A8\u09BF\u0995\u09CB\u09A1 \u0985\u0995\u09CD\u09B7\
  \u09B0\u2026"
lastmod: '2024-04-05T22:40:37.452987-06:00'
model: gpt-4-0125-preview
summary: "\u0990\u09A4\u09BF\u09B9\u09BE\u09B8\u09BF\u0995\u09AD\u09BE\u09AC\u09C7\
  , `len()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u09DF\u09C7\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\u09CB\u0981\
  \u099C\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09BE\u0987\u09A5\u09A8\u09C7\
  \u09B0 \u09AA\u09CD\u09B0\u09BF\u09DF \u0989\u09AA\u09BE\u09DF\u0964 \u098F\u099F\
  \u09BF \u09AE\u09BE\u09B0\u09CD\u099C\u09BF\u09A4 \u098F\u09AC\u0982 \u09A6\u09CD\
  \u09B0\u09C1\u09A4\u0964 \u09AA\u09BE\u0987\u09A5\u09A8\u09C7\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09CB \u0987\u0989\u09A8\u09BF\u0995\
  \u09CB\u09A1 \u0985\u0995\u09CD\u09B7\u09B0 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\
  \u09BF\u09A7\u09BF\u09A4\u09CD\u09AC\u0995\u09BE\u09B0\u09C0 \u09AC\u09BE\u0987\u099F\
  \u09C7\u09B0 \u0985\u09CD\u09AF\u09BE\u09B0\u09C7 \u09B9\u09BF\u09B8\u09BE\u09AC\
  \u09C7 \u09A5\u09BE\u0995\u09C7, \u098F\u09AC\u0982 `len()` \u0993\u0987\u0997\u09C1\
  \u09B2\u09CB \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09C7\u0964 \u098F\u0987 \u09AB\
  \u09BE\u0982\u09B6\u09A8\u099F\u09BF \u09B6\u09C1\u09A7\u09C1 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09A8\u09DF\
  , \u09AF\u09C7\u0995\u09CB\u09A8\u09CB \u0987\u099F\u09BE\u09B0\u09C7\u09AC\u09B2\
  \u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\u0964\
  \ \u09AC\u09BF\u0995\u09B2\u09CD\u09AA?"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
```python
# len() ফাংশনের সরল ব্যবহার
my_string = "Hello, World!"
length = len(my_string)
print(length)  # আউটপুট: 13

# লুপে দৈর্ঘ্য
for i in range(len(my_string)):
    print(my_string[i], end='')  # আউটপুট: Hello, World!
print()  # নতুন লাইনের জন্য

# স্ট্রিং দৈর্ঘ্যের সাথে অন্যান্য অপারেশন যুক্ত করা
if len(my_string) > 10:
    print("এটি একটি দীর্ঘ স্ট্রিং!")  # আউটপুট: এটি একটি দীর্ঘ স্ট্রিং!
```

## গভীর ডুব
ঐতিহাসিকভাবে, `len()` ফাংশন স্ট্রিংয়ের দৈর্ঘ্য খোঁজার জন্য পাইথনের প্রিয় উপায়। এটি মার্জিত এবং দ্রুত। পাইথনের স্ট্রিংগুলো ইউনিকোড অক্ষর প্রতিনিধিত্বকারী বাইটের অ্যারে হিসাবে থাকে, এবং `len()` ওইগুলো গণনা করে। এই ফাংশনটি শুধু স্ট্রিংয়ের সাথে নয়, যেকোনো ইটারেবলের সাথে কাজ করে।

বিকল্প? ভাল, স্ট্রিংয়ের জন্য সাধারণত ব্যবহৃত নয়, কিন্তু আপনি ম্যানুয়ালি একটি স্ট্রিং প্রতি লুপ চালিয়ে অক্ষর গণনা করতে পারেন—বেমানান এবং অকার্যকর। ইউনিকোড সাপোর্টের আগে, একটি স্ট্রিংয়ের দৈর্ঘ্য কখনো কখনো এর মেমরি আকার থেকে ভিন্ন হতো, কিন্তু যেহেতু পাইথন 3'র স্ট্রিংগুলো ইউনিকোড-নেটিভ, `len()` সঠিকভাবে অক্ষরের সংখ্যাকে প্রতিনিধিত্ব করে।

বাস্তবায়নের দিক থেকে, পাইথনের স্ট্রিংগুলো মেটাডেটা সহ অবজেক্ট, যাতে দৈর্ঘ্য অন্তর্ভুক্ত থাকে, তাই `len()` আসলে একটি O(1) অপারেশন—স্থির সময়, স্ট্রিংয়ের আকার যাই হোক না কেন। এটি যেন আপনার আঙ্গুল ফাঁকি দিয়ে উত্তর পাওয়া।

## দেখুন
- `len()` জন্য পাইথন ডকুমেন্টেশন: https://docs.python.org/3/library/functions.html#len
- পাইথনে ইউনিকোড এবং স্ট্রিং এনকোডিং: https://docs.python.org/3/howto/unicode.html
- বিল্ট-ইন টাইপগুলির জন্য পাইথনের সময় জটিলতা: https://wiki.python.org/moin/TimeComplexity
