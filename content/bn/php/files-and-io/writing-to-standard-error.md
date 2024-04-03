---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:41.900004-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP-\u09A4\u09C7, stderr-\u098F\
  \ \u09B2\u09C7\u0996\u09BE \u0985\u09B0\u09CD\u099C\u09BF\u09A4 \u09B9\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7 `fwrite()` \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AF\u09BE\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09AA\u09C2\u09B0\u09CD\u09AC\u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09BF\
  \u09A4 \u09B8\u09CD\u09A5\u09BF\u09B0\u09BE\u0999\u09CD\u0995 `STDERR` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC, \u09AF\u09BE \u09A4\u09CD\u09B0\
  \u09C1\u099F\u09BF \u0986\u0989\u099F\u09AA\u09C1\u099F\u2026"
lastmod: '2024-03-17T18:47:44.149347-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7, stderr-\u098F \u09B2\u09C7\u0996\u09BE \u0985\u09B0\u09CD\
  \u099C\u09BF\u09A4 \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7 `fwrite()` \u09AB\
  \u09BE\u0982\u09B6\u09A8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09AF\u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AA\u09C2\u09B0\u09CD\u09AC\u09A8\
  \u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09BF\u09A4 \u09B8\u09CD\u09A5\u09BF\u09B0\u09BE\
  \u0999\u09CD\u0995 `STDERR` \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\
  \u09BC, \u09AF\u09BE \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u0986\u0989\u099F\u09AA\
  \u09C1\u099F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u0995\u09C7 \u09AA\u09CD\
  \u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\u09CD\u09AC \u0995\u09B0\u09C7\u0964\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
PHP-তে, stderr-এ লেখা অর্জিত হতে পারে `fwrite()` ফাংশনের মাধ্যমে যার সাথে পূর্বনির্ধারিত স্থিরাঙ্ক `STDERR` ব্যবহৃত হয়, যা ত্রুটি আউটপুট স্ট্রিমকে প্রতিনিধিত্ব করে।

```php
<?php
// stderr-এ একটি সরল বার্তা লেখা।
fwrite(STDERR, "এটি একটি ত্রুটি বার্তা।\n");
```

যখন স্ক্রিপ্টটি কমান্ড লাইন থেকে চালানো হয় তখন নমুনা আউটপুট:
```
এটি একটি ত্রুটি বার্তা।
```

আরও বাস্তবসম্মত ব্যবহার দেখানোর জন্য, এমন একটি পরিস্থিতি বিবেচনা করুন যেখানে আপনি ব্যবহারকারীর ইনপুট পার্স করছেন এবং অপ্রত্যাশিত ডেটা পাচ্ছেন:
```php
<?php
$input = 'unexpected data';

// ব্যবহারকারীর ইনপুট প্রসেসিংয়ে ত্রুটি সিমুলেশন করা।
if ($input === 'unexpected data') {
    fwrite(STDERR, "ত্রুটি: অপ্রত্যাশিত ইনপুট প্রাপ্ত হয়েছে।\n");
    exit(1); // একটি অ-শূন্য মান দিয়ে প্রস্থান করা যা একটি ত্রুটিকে নির্দেশ করে।
}
```

PHP-এর নিজস্ব stderr হ্যান্ডেলিং ক্ষমতা সাধারণত যথেষ্ট হলেও, আরও জটিল অ্যাপ্লিকেশন সম্পর্কে কাজ করার সময় বা বাহ্যিক সিস্টেমগুলির সাথে stderr লগিং একীভূত করার ক্ষেত্রে, Monolog মতো থার্ড-পার্টি লাইব্রেরিগুলি একটি শক্তিশালী মিত্র হতে পারে। Monolog একটি লগিং লাইব্রেরি যা stderr-এর মধ্যে অনেক অন্যান্য লক্ষ্য (ফাইল, সকেট, ইত্যাদি) সামলাতে পারে।

Monolog ব্যবহার করে stderr-এ লেখাঃ

প্রথমে, নিশ্চিত করুন যে আপনার কাছে Composer মাধ্যমে Monolog ইনস্টল করা আছে:
```
composer require monolog/monolog
```

তারপর, আপনি `php://stderr`-এ লক্ষ্য করে `StreamHandler`-কে কনফিগার করতে Monolog ব্যবহার করতে পারেন:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// একটি লগ চ্যানেল তৈরি করা
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// stderr-এ একটি লগ বার্তা যোগ করা
$log->warning('এটি একটি সতর্কবার্তা।');
```

উপরের কোড Monolog ব্যবহার করে stderr-এ একটি সতর্কবার্তা পাঠায়, যা বিশেষত সেই অ্যাপ্লিকেশনের জন্য উপযোগী যা বিস্তারিত লগিং কনফিগারেশন অথবা বাহ্যিক লগ মনিটরিং প্রয়োজন।
