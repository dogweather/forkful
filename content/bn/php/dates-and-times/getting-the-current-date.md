---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:49.349037-06:00
description: "PHP-\u09A4\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u0995\u09BE\u099C \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\
  \u09C7 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0993 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE \u098F\u09AC\u0982\
  \ \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09B2\u0997\u09BF\u0982, \u09B8\u09AE\
  \u09AF\u09BC-\u09AE\u09C1\u09A6\u09CD\u09B0\u09BE\u0999\u09CD\u0995\u09A8 \u09AA\
  \u09CB\u09B8\u09CD\u099F, \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\u09AE\u09AF\
  \u09BC\u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u2026"
lastmod: '2024-03-17T18:47:44.142382-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AA\u09C7\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u0995\u09BE\u099C \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u0995\
  \u09C7 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u0993 \u09B8\u09AE\u09AF\u09BC \u0986\u09A8\u09BE \u098F\u09AC\u0982\
  \ \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\
  \u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09B2\u0997\u09BF\u0982, \u09B8\u09AE\
  \u09AF\u09BC-\u09AE\u09C1\u09A6\u09CD\u09B0\u09BE\u0999\u09CD\u0995\u09A8 \u09AA\
  \u09CB\u09B8\u09CD\u099F, \u0987\u09AD\u09C7\u09A8\u09CD\u099F \u09B8\u09AE\u09AF\
  \u09BC\u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কি ও কেন?
PHP-তে বর্তমান তারিখ পেতে একটি মৌলিক কাজ যা আপনাকে সিস্টেমের তারিখ ও সময় আনা এবং ম্যানিপুলেট করতে দেয়। লগিং, সময়-মুদ্রাঙ্কন পোস্ট, ইভেন্ট সময়নির্ধারণ বা আপনার অ্যাপ্লিকেশনে সময়-সংবেদনশীল অপারেশন সঞ্চালনের মতো ফাংশনের জন্য এটি জরুরী।

## কিভাবে:
### নেটিভ PHP
PHP-এর অন্তর্নির্মিত `date()` ফাংশনটি বর্তমান তারিখ পাওয়ার সবচেয়ে সরাসরি উপায়। আপনি ফর্ম্যাট প্যারামিটার নির্দিষ্ট করে বিভিন্ন উপায়ে তারিখ ফর্ম্যাট করতে পারেন।

```php
echo date("Y-m-d"); // আউটপুট: 2023-04-01 (উদাহরণস্বরূপ)
echo date("l, F j, Y"); // আউটপুট: Saturday, April 1, 2023
```

টাইমজোন সাপোর্ট সহ তারিখ এবং সময় পেতে, আপনি `DateTime` ক্লাস এবং `DateTimeZone` ব্যবহার করতে পারেন।

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // আউটপুট: 2023-04-01 12:00:00 (উদাহরণস্বরূপ)
```

### কার্বন ব্যবহার করে (একটি জনপ্রিয় তৃতীয় পক্ষের লাইব্রেরি)
[কার্বন](https://carbon.nesbot.com/) `DateTime`-এর জন্য একটি সহজ API এক্সটেনশন যা তারিখ ও সময়ের সঙ্গে কাজ করার জন্য আরও পরিষ্কার ও আরও নির্মল উপায় প্রদান করে।

প্রথমে, নিশ্চিত করে নিন যে আপনি কার্বন Composer এর মাধ্যমে ইনস্টল করেছেন:
```bash
composer require nesbot/carbon
```

তারপর, আপনি বর্তমান তারিখ পেতে এটি ব্যবহার করতে পারেন:

```php
use Carbon\Carbon;

echo Carbon::now(); // আউটপুট: 2023-04-01 12:00:00 (উদাহরণস্বরূপ, ডিফল্ট ফর্ম্যাটে)
echo Carbon::now()->toDateString(); // আউটপুট: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // আউটপুট: Saturday, April 1, 2023
```

কার্বন PHP-তে তারিখ-সময় হাতড়ানোকে পঠনযোগ্যতা এবং সময় ম্যানিপুলেশন, তুলনা, এবং ফর্ম্যাটিংয়ের জন্য প্রচুর কার্যকারিতা যুক্ত করে সমৃদ্ধ করে।
