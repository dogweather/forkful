---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:51.164673-06:00
description: "\u098F\u0995\u099F\u09BF \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\
  \ \u09AC\u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\
  \u09AE\u09AF\u09BC \u09AC\u09CD\u09AF\u09AC\u09A7\u09BE\u09A8\u09C7\u09B0 \u0986\
  \u0997\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u0996\u09C1\u0981\u099C\u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u09B0\u09BF\u09AE\u09BE\u0987\u09A8\u09CD\u09A1\u09BE\
  \u09B0, \u09AE\u09C7\u09AF\u09BC\u09BE\u09A6\u2026"
lastmod: '2024-03-17T18:47:43.588857-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\
  \u09BE \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\
  \u09BF\u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\
  \u09AF\u09BC \u09AC\u09CD\u09AF\u09AC\u09A7\u09BE\u09A8\u09C7\u09B0 \u0986\u0997\
  \u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0996\u09C1\u0981\u099C\u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u09B0\u09BF\u09AE\u09BE\u0987\u09A8\u09CD\u09A1\u09BE\u09B0\
  , \u09AE\u09C7\u09AF\u09BC\u09BE\u09A6\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কী ও কেন?
একটি ভবিষ্যত বা অতীতের তারিখ হিসেব করা মানে একটি নির্দিষ্ট সময় ব্যবধানের আগে বা পরে একটি তারিখ খুঁজে পাওয়া। প্রোগ্রামাররা এটি রিমাইন্ডার, মেয়াদ উত্তির্ণ তারিখ, সময়সূচি নির্ধারণ বা সময়-নির্ভর গণনা এর জন্য করে থাকেন।

## কীভাবে:
Python-এর `datetime` মডিউল তারিখ এবং সময় নিয়ে কাজ করা অনেক সহজ করে দেয়। এটা দেখুন:

```Python
from datetime import datetime, timedelta

# বর্তমান তারিখ ও সময়
now = datetime.now()
print("এখন: ", now)

# ১০ দিন যোগ 
future_date = now + timedelta(days=10)
print("ভবিষ্যতের তারিখ (+১০ দিন): ", future_date)

# ৫ দিন বিয়োগ
past_date = now - timedelta(days=5)
print("অতীতের তারিখ (-৫ দিন): ", past_date)
```
আউটপুট দেখতে এমন হতে পারে:
```
এখন: ২০২৩-০৪-০১ ১২:৩৪:৫৬.৭৮৯০১২
ভবিষ্যতের তারিখ (+১০ দিন): ২০২৩-০৪-১১ ১২:৩৪:৫৬.৭৮৯০১২
অতীতের তারিখ (-৫ দিন): ২০২৩-০৩-২৭ ১২:৩৪:৫৬.৭৮৯০১২
```

সহজ, তাই না? শুধু দিনগুলো পরিবর্তন করুন, অথবা `timedelta`-এ `সপ্তাহ`, `ঘন্টা`, `মিনিট`, বা `সেকেন্ড` ব্যবহার করে আপনার প্রয়োজনীয় সময়ে যান।

## গভীর ডুব
বহু আগে, তারিখ ও সময় গণনা করা ছিল একটি কঠিন ব্যাপার। আপনি লিপ বছর, সময়ক্ষেত্র, দিবালোক সঞ্চয়ের সময় - এক গোলমালের মধ্যে পড়তেন। Python-এর `datetime` এবং এর সঙ্গী `date` এবং `time` দিয়ে, এটি নিখুঁত ভাবে কাজ করা যায়। মডিউলটি পিছনে এই জটিলতাগুলি সামলায়।

প্রতিস্থাপন সম্পর্কে আপনি জিজ্ঞাসা করতে পারেন। অবশ্যই। `dateutil` মতো লাইব্রেরিগুলি আরও জটিল তারিখ পরিবর্তনগুলি এবং পার্সিং সামাল দিতে পারে। `datetime` যখন পূরণ করতে পারে না তখন এটি যেতে হবে।

বাস্তবায়নের দিক থেকে, আপনি যখন `timedelta` ব্যবহার করেন, Python লিপ বছর এবং এই ধরনের বিষয় বিবেচনায় নিয়ে তারিখ সমন্বয় করে। তবে সর্বদা আপনার ফলাফলগুলি পরীক্ষা করুন - বিশেষ করে সময়ক্ষেত্রের সাথে ডিল করার সময়। এবং মনে রাখবেন, `datetime` ডিফল্টরূপে নির্বিচার; আপনি যদি না বলেন তা সে সময়ক্ষেত্রের বিষয়ে বিবেচনা করে না।

## আরও দেখুন
- Python-এর `datetime` ডকুমেন্টেশন: https://docs.python.org/3/library/datetime.html
- `dateutil` লাইব্রেরি: https://dateutil.readthedocs.io/en/stable/
- Python-এ সময়ক্ষেত্র হ্যান্ডলিং: https://docs.python.org/3/library/zoneinfo.html
