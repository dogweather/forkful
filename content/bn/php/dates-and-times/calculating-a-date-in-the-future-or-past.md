---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:30.484892-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP `DateTime` \u098F\u09AC\u0982\
  \ `DateInterval` \u098F\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09BE\
  \u09B0\u09BF\u0996\u09C7\u09B0 \u0997\u09BE\u09A3\u09BF\u09A4\u09BF\u0995 \u0995\
  \u09BE\u099C\u0995\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\
  \u09C7\u0964 \u09A6\u09C7\u0996\u09C1\u09A8."
lastmod: '2024-03-17T18:47:44.145884-06:00'
model: gpt-4-0125-preview
summary: "PHP `DateTime` \u098F\u09AC\u0982 `DateInterval` \u098F\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u0997\
  \u09BE\u09A3\u09BF\u09A4\u09BF\u0995 \u0995\u09BE\u099C\u0995\u09C7 \u09B8\u09B9\
  \u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u09A6\u09C7\u0996\u09C1\
  \u09A8."
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কিভাবে:
PHP `DateTime` এবং `DateInterval` এর মাধ্যমে তারিখের গাণিতিক কাজকে সহজ করে তোলে। দেখুন:

```PHP
<?php
// আজকের তারিখ
$today = new DateTime();
echo $today->format('Y-m-d H:i:s') . "\n";

// ১০ দিন যোগ
$today->add(new DateInterval('P10D'));
echo $today->format('Y-m-d H:i:s') . "\n";

// ২ মাস বিয়োগ
$today->sub(new DateInterval('P2M'));
echo $today->format('Y-m-d H:i:s') . "\n";
?>
```
আউটপুট হতে পারে:
```
2023-04-01 12:34:56
2023-04-11 12:34:56
2023-02-11 12:34:56
```

## গভীরে ডুব:
পূর্বে, PHP তে তারিখের হিসেব আরও ভুলপ্রবণ ছিল। `strtotime`, যা এখনও উপযোগী, কিন্তু কোণঠাসা কেসে আপনাকে বিপদে ফেলতে পারে। `DateTime` এবং `DateInterval` নির্ভুলতা এবং বস্তু-ভিত্তিক স্পষ্টতা নিয়ে এসেছে।

বিকল্প? নিশ্চয়ই। Carbon এর মতো লাইব্রেরি PHP-র তারিখের কার্যকারিতাকে আরও পাঠযোগ্যতা এবং বৈশিষ্ট্যের জন্য জড়িয়ে রাখে, কিন্তু অনেক ক্ষেত্রে, PHP-র নিজের ক্লাসগুলোই সম্পূর্ণ ভাবে কাজ করবে।

প্রযুক্তিগত দিক থেকে, `DateTime::add()` এবং `DateTime::sub()` অবজেক্টটি পরিবর্তন করে, তাই পুনরায় অ্যাসাইন করার প্রয়োজন নেই। তারা সময়ের এককগুলিকে সামঞ্জস্যপূর্ণভাবে সামলানো, লিপ বছর এবং ডেলাইট সেভিং সময় পরিবর্তনগুলির মতো বিষয়গুলি হিসাবে নেয়, যা অন্যথায় আসলে একটি মাথাব্যথার কারণ হতে পারে।

## দেখুন আরও
- PHP ম্যানুয়াল DateTime এ: https://www.php.net/manual/en/class.datetime.php
- DateInterval ডকুমেন্টেশন: https://www.php.net/manual/en/class.dateinterval.php
- Carbon: DateTime এর জন্য একটি সহজ API এক্সটেনশন - https://carbon.nesbot.com
