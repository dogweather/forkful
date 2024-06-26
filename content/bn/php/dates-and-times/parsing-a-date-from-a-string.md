---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:13.279817-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP \u098F\u09B0 \u0985\u09A8\u09CD\
  \u09A4\u09B0\u09CD\u09A8\u09BF\u09B9\u09BF\u09A4 `DateTime` \u0995\u09CD\u09B2\u09BE\
  \u09B8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\
  \u0982 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\
  \u0995\u099F\u09BF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u09AB\
  \u09BE\u0982\u09B6\u09A8 \u09B8\u09AE\u09C2\u09B9 \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u098F\u0995\u099F\u09BF\
  \ \u09A4\u09BE\u09B0\u09BF\u0996 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.141375-06:00'
model: gpt-4-0125-preview
summary: "PHP \u098F\u09B0 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B9\u09BF\
  \u09A4 `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B6\u0995\u09CD\
  \u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09AE\
  \u09C2\u09B9 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u0986\
  \u09AA\u09A8\u09BF \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7 `DateTime` \u0987\u09A8\u09CD\u09B8\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09B8 \u09A4\u09C8\u09B0\u09C0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8 \u098F\u09AC\u0982 \u09A4\u09BE\u09B0\u09AA\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09AE\u09A4\u09CB \u09AB\u09B0\u09AE\u09CD\
  \u09AF\u09BE\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0987 \u09B0\u0995\u09AE \u0995\u09BF\
  \u09AD\u09BE\u09AC\u09C7."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
PHP এর অন্তর্নিহিত `DateTime` ক্লাস তারিখ পার্স এবং কাজ করার জন্য একটি শক্তিশালী ফাংশন সমূহ প্রদান করে। আপনি একটি তারিখ স্ট্রিং ব্যবহার করে `DateTime` ইন্সট্যান্স তৈরী করতে পারেন এবং তারপর প্রয়োজন মতো ফরম্যাটে পরিণত করতে পারেন। এই রকম কিভাবে:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// আউটপুট: 2023-04-25 15:30:00
```

যে স্ট্রিংগুলো মানক বিন্যাস অনুসরণ না করে, তা সামলাতে আপনি `createFromFormat` মেথড ব্যবহার করতে পারেন, যা আপনাকে ইনপুট তারিখের সঠিক বিন্যাস নির্দিষ্ট করতে দেয়:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// আউটপুট: 2023-04-25 15:30:00
```

`DateTime` দ্বারা সরাসরি সমর্থিত না হওয়া আরও জটিল পার্সিং কাজের জন্য, PHP `strtotime` ফাংশন অফার করে, যা যেকোনো ইংরেজি টেক্সটুয়াল ডেটটাইম বর্ণনাকে একটি ইউনিক্স টাইমস্ট্যাম্পে পার্স করার চেষ্টা করে:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// আউটপুট বর্তমান তারিখ অনুযায়ী ভিন্ন হবে, যেমন, "2023-05-04"
```

**তৃতীয়-পক্ষের লাইব্রেরী ব্যবহার করে:**

যদিও PHP এর অন্তর্নিহিত ফাংশনগুলো অনেক ব্যবহার ক্ষেত্র নির্বাচন করে, কখনো কখনো আপনার আরও উন্নত পার্সিং ক্ষমতা প্রয়োজন পড়তে পারে। কার্বন লাইব্রেরী, PHP এর DateTime ক্লাসের একটি এক্সটেনশন, তারিখ/সময় পরিচালনার জন্য একটি সমৃদ্ধ ফিচার সেট প্রদান করে:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// আউটপুট ভিন্ন হবে, যেমন, "2023-04-26 00:00:00"
```

কার্বনের `parse` মেথড নানান ধরনের তারিখ এবং সময় বিন্যাসের সাথে বুদ্ধিমানভাবে কাজ করতে পারে, যা নমনীয় তারিখ পার্সিং কার্যকারিতা প্রয়োজনীয় অ্যাপ্লিকেশনের জন্য একটি অমূল্য টুল করে তোলে।
