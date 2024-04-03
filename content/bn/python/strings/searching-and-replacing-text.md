---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:44.588431-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:43.555657-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কীভাবে:
```Python
# সিম্পল রিপ্লেসমেন্টের জন্য str.replace() ব্যবহার করা
text = "I like Python. Python is awesome!"
text = text.replace("Python", "programming")
print(text)  # আউটপুট: I like programming. programming is awesome!

# রেগেক্সের সাথে প্যাটার্ন-ভিত্তিক রিপ্লেসমেন্টের জন্য re.sub() ব্যবহার করা
import re
text = "Contact us at support@example.com"
new_text = re.sub(r'\b[a-zA-Z0-9.-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}\b', 'support@newdomain.com', text)
print(new_text)  # আউটপুট: Contact us at support@newdomain.com
```

## গভীরে ডাইভ
প্রোগ্রামিংয়ের আদিকালে, টেক্সট সম্পাদনা ছিল একটি ম্যানুয়াল ধৈর্যের কাজ। এখানে রেগেক্স (রেগুলার এক্সপ্রেশন) প্রবেশ করে, ১৯৫০-এর দশকে নির্মিত, যা অনুসন্ধান প্রক্রিয়াকে কম মাথাব্যথাকার বিষয়ে পরিণত করে। সিম্পল প্রতিস্থাপনের জন্য, `str.replace()` আপনার প্রথম পছন্দ। এটি সোজা এবং একবারের প্রতিস্থাপনের জন্য দারুণ। যখন আপনার কাছে প্যাটার্ন থাকে, যেমন ফোন নম্বর, ইমেইল, অথবা তারিখ, `re.sub()` সঙ্গে রেগেক্স হলো যাদুর লাঠি। এটি একটি বিশেষ সিনট্যাক্সের সাহায্যে প্যাটার্ন খুঁজে পায় এবং তাদের বদলে দেয়। মনে রাখবেন, রেগেক্স যেমন শক্তিশালী, তেমনি বিচিত্র ও হতে পারে; এটি এমন একটি টুল যেখানে আপনি আরো বেশি ধাঁধা সমাধান করলে আরো ভালো হয়ে উঠবেন।

## আরো দেখুন
- [Python `str.replace()` ডকুমেন্টেশন](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python `re` মডিউল ডকুমেন্টেশন](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/): অনলাইনে রেগেক্স প্যাটার্নের পরীক্ষা করতে
- [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/): একটি বই যেখানে আপনি প্র্যাকটিকাল টেক্সট প্রক্রিয়াকরণ কার্যক্রম সম্পর্কে আরো জানতে পারেন।
