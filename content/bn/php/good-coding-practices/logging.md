---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:33.452176-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP-\u098F \u098F\u0995\u099F\u09BF\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u098F\u09B0\u09B0 \u09B2\u0997\u09BF\
  \u0982 \u09AB\u09BE\u0982\u09B6\u09A8 \u0986\u099B\u09C7 \u09AF\u09BE \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B8\u09B9\u099C\u0964 \u09B6\
  \u09C1\u09A7\u09C1 `error_log()` \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\
  \u09C7 \u09A2\u09C1\u0995\u09BF\u09AF\u09BC\u09C7 \u09A6\u09BF\u09A8 \u09A4\u09BE\
  \u09B9\u09B2\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09BE\u09B0\u09CD\u09AD\
  \u09BE\u09B0 \u09B2\u0997\u09C7 \u098F\u0995\u099F\u09BF \u09AE\u09C7\u09B8\u09C7\
  \u099C \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB\u2026"
lastmod: '2024-03-17T18:47:44.137718-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u098F \u098F\u0995\u099F\u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\
  \u09A8 \u098F\u09B0\u09B0 \u09B2\u0997\u09BF\u0982 \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u0986\u099B\u09C7 \u09AF\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE \u09B8\u09B9\u099C\u0964 \u09B6\u09C1\u09A7\u09C1 `error_log()` \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7 \u09A2\u09C1\u0995\u09BF\u09AF\
  \u09BC\u09C7 \u09A6\u09BF\u09A8 \u09A4\u09BE\u09B9\u09B2\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u09B2\u0997\u09C7 \u098F\
  \u0995\u099F\u09BF \u09AE\u09C7\u09B8\u09C7\u099C \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB \u09B9\u09AC\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u0995\
  \u09BE\u09B8\u09CD\u099F\u09AE\u09BE\u0987\u099C \u0995\u09B0\u09C7 \u098F\u0995\
  \u099F\u09BF \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AB\u09BE\
  \u0987\u09B2\u09C7 \u09B2\u09BF\u0996\u09A4\u09C7\u0993 \u09AA\u09BE\u09B0\u09C7\
  \u09A8\u0964."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
PHP-এ একটি বিল্ট-ইন এরর লগিং ফাংশন আছে যা ব্যবহার করা সহজ। শুধু `error_log()` আপনার কোডে ঢুকিয়ে দিন তাহলে আপনার সার্ভার লগে একটি মেসেজ পাঠানো হবে। আপনি এটি কাস্টমাইজ করে একটি নির্দিষ্ট ফাইলে লিখতেও পারেন।

```php
<?php
// একটি সহজ তথ্য মেসেজ লগ করা
error_log("This is an info log entry.");

// একটি এরর মেসেজ লগ করা
error_log("This is an error log entry.", 0);

// একটি নির্দিষ্ট ফাইলে লগ করা
file_put_contents('/path/to/your/custom.log', "A custom log entry.\n", FILE_APPEND);

// স্ট্রাকচারড লগিং এর জন্য Monolog ব্যবহার করা
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// লগার তৈরি করা
$logger = new Logger('name');
// এখন কিছু হ্যান্ডলার যোগ করুন
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// এখন আপনি আপনার লগার ব্যবহার করতে পারেন
$logger->warning('This is a warning log!');
$logger->error('This is an error log!');
?>
```

এতে আপনার লগগুলি সার্ভার লগে বা আপনার নির্দিষ্ট ফাইলে প্লেইন টেক্সট ফর্ম্যাটে আউটপুট দেবে।

## গভীর ডুব:
প্রাকৃতিকভাবে, PHP ডেভেলপাররা `error_log()` ফাংশন অথবা Apache/Nginx লগগুলির উপর নির্ভর করতেন তাদের সমস্যাগুলি ধরার জন্য, কিন্তু সেটি প্লেইন টেক্সট ফাইলগুলি পার্স করার প্রয়োজন এবং তাদের ফিল্টার বা সর্ট করার সহজ উপায় না থাকায় ক্যাওসের মতো হতে পারে। এরপর লগিং লাইব্রেরি Monolog এর মতো সমাধান এসেছে, যা PHP-এ স্ট্রাকচারড লগিং এর যুগ শুরু করে। এই সমাধানগুলি আপনাকে বিভিন্ন লগিং চ্যানেল, গুরুত্বের স্তর, ও সাজানো আউটপুট (যেমন JSON, যা প্রোগ্রামেটিকভাবে পার্স করার জন্য স্বপ্নের মতো) অফার করে আরও বেশি নিয়ন্ত্রণ দেয়।

Monolog ছাড়াও বিকল্পগুলি ভিতরে আছে Log4php, KLogger, এবং Apache's Log4php। বাস্তবায়নের দিক থেকে, শক্তিশালী লগিং প্রয়োজন শুধু ডেটা যেখানে সেখানে ফেলা নয়, বরং লগ রোটেশন, আর্কাইভাল স্ট্র্যাটেজি, এবং মনিটরিং টুলসের সাথে ইন্টিগ্রেশনের মতো বিষয়গুলি বিবেচনা করা।

আপনার [PSR-3 লগার ইন্টারফেস](https://www.php-fig.org/psr/psr-3/) মনে রাখা উচিত, যা লগিং লাইব্রেরিগুলির জন্য একটি সাধারণ ইন্টারফেস বর্ণনা করে, ইন্টারঅপারেবিলিটি এবং লগিং মেকানিজমে অ্যাক্সেস করার জন্য একটি ধারাবাহিক উপায় নিশ্চিত করে।

## আরও দেখুন:
- [Monolog GitHub রিপজিটরি](https://github.com/Seldaek/monolog)
- [PSR-3 লগার ইন্টারফেস স্পেসিফিকেশন](https://www.php-fig.org/psr/psr-3/)
- [PHP এরর লগ ডকুমেন্টেশন](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: PHP এর জন্য একটি সহজ লগিং ক্লাস](https://github.com/katzgrau/KLogger)
- [Log4php: একটি বহুমুখী লগিং ফ্রেমওয়ার্ক ফর PHP](https://logging.apache.org/log4php/)

বিল্ট-ইন ফাংশনগুলির সাথে শুরু করুন, কিন্তু একটি আরও রেখেদেখে ও স্কেলযোগ্য পদ্ধতির জন্য, Monolog এর মতো একটি লাইব্রেরির সাথে সময় বিনিয়োগ করার কথা বিবেচনা করুন। সুখী লগিং!
