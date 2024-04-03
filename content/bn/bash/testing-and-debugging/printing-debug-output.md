---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:48.845389-06:00
description: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F\
  \ \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u09B2 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A1\u09C7\u099F\u09BE\
  \ \u098F\u0995\u09CB \u0995\u09B0\u09BE \u09AF\u09BE\u09A4\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 \u0995\
  \u09BF \u0998\u099F\u099B\u09C7 \u09A4\u09BE \u099A\u09C7\u0995 \u0995\u09B0\u09BE\
  \ \u09AF\u09BE\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\
  \u09C7\u09A8 \u09AD\u09C7\u09B0\u09BF\u09DF\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF\
  \ \u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995 \u0995\u09B0\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.229807-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09B9\u09B2 \u0995\u09A8\u09B8\u09CB\u09B2\u09C7 \u09A1\u09C7\u099F\u09BE \u098F\
  \u0995\u09CB \u0995\u09B0\u09BE \u09AF\u09BE\u09A4\u09C7 \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09C7 \u0995\u09BF\
  \ \u0998\u099F\u099B\u09C7 \u09A4\u09BE \u099A\u09C7\u0995 \u0995\u09B0\u09BE \u09AF\
  \u09BE\u09DF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\
  \ \u09AD\u09C7\u09B0\u09BF\u09DF\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u099F\
  \u09CD\u09B0\u09CD\u09AF\u09BE\u0995 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, \u09B2\u099C\u09BF\u0995 \u09AB\u09CD\u09B2\u09CB \u0985\u09A8\u09C1\u09B8\
  \u09B0\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u098F\u09AC\u0982\
  \ \u09AC\u09BF\u09B0\u0995\u09CD\u09A4\u09BF\u0995\u09B0 \u09AC\u09BE\u0997\u0997\
  \u09C1\u09B2\u09BF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF\u0964."
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কি এবং কেন?

ডিবাগ আউটপুট প্রিন্ট করা মানে হল কনসোলে ডেটা একো করা যাতে আপনার স্ক্রিপ্টে কি ঘটছে তা চেক করা যায়। প্রোগ্রামাররা এটি করে থাকেন ভেরিয়েবলগুলি ট্র্যাক করার জন্য, লজিক ফ্লো অনুসরণ করার জন্য, এবং বিরক্তিকর বাগগুলি চিহ্নিত করার জন্য।

## কিভাবে:

```Bash
#!/bin/bash

# একটি ভেরিয়েবল সংজ্ঞায়িত করুন
name="Gizmo"

# ডিবাগিং এর জন্য ভেরিয়েবল প্রিন্ট করুন
echo "Debug: ভেরিয়েবলের নাম হল $name"

# ডিবাগ আউটপুটের সাথে কন্ডিশনাল
if [[ $name == "Gizmo" ]]; then
    echo "Debug: if-স্টেটমেন্টে প্রবেশ করেছে।"
    # কিছু একটা করুন
fi

# ডিবাগ আউটপুটের সাথে লুপ
for i in {1..3}; do
    echo "Debug: লুপ ইটারেশন $i"
    # লুপে কিছু একটা করুন
done
```

আউটপুট:
```
Debug: ভেরিয়েবলের নাম হল Gizmo
Debug: if-স্টেটমেন্টে প্রবেশ করেছে।
Debug: লুপ ইটারেশন 1
Debug: লুপ ইটারেশন 2
Debug: লুপ ইটারেশন 3
```

## গভীর ডাইভ

মূলত, ডিবাগিং মানে ছিল শারীরিক বাগগুলি অপসারণ করা যা প্রারম্ভিক কম্পিউটারগুলিতে বিঘ্ন ঘটাত। বর্তমানে, এটি কোড বাগগুলি স্কোয়াশ করার বিষয়ে। ডিবাগ আউটপুটগুলি হলো প্রোগ্রামারের আবর্ধককাচ।

ব্যাশ স্ক্রিপ্টে `echo` এর পরিবর্তে `printf` রয়েছে আরও ফর্ম্যাটিং অপশনের জন্য অথবা স্থায়ী লগের জন্য পুনঃনির্দেশন `>` এর মাধ্যমে ফাইলে লেখা।

ব্যাশ পূর্ণ-স্ক্রিপ্ট ডিবাগিংয়ের জন্য নির্মিত কমান্ডগুলি এবং তাদের আর্গুমেন্টগুলি যেমন নির্বাহিত হয় তা ট্রেস করার জন্য `set -x` সমর্থন করে। `set -x` পূর্ণ-স্ক্রিপ্ট ডিবাগিংয়ের জন্য দারুণ।

## আরো দেখুন

- ব্যাশের `man` পেজ: `man bash`
- এডভান্সড স্ক্রিপ্টিং গাইড: [Bash Guide for Beginners by Machtelt Garrels](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- ট্রাবলশুটিং এর জন্য স্ট্যাক ওভারফ্লো: [stackoverflow.com](https://stackoverflow.com/questions/tagged/bash)
