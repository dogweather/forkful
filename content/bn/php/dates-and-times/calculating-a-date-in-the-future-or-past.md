---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:30.484892-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09B9\u09BF\u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\
  \u09BC\u09C7\u09B0 \u0986\u0997\u09C7 \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0996\u09C1\u0981\u099C\
  \u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u0985\u09CD\u09AF\u09BE\u09AA\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B0\u09BF\u09AE\u09BE\u0987\u09A8\
  \u09CD\u09A1\u09BE\u09B0, \u09B8\u09BE\u09AC\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09B6\u09A8, \u09B6\u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982 \u098F\u09AC\
  \u0982\u2026"
lastmod: '2024-03-17T18:47:44.145884-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE \u0985\
  \u09A4\u09C0\u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\
  \u0996 \u09B9\u09BF\u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\u09BC\
  \u09C7\u09B0 \u0986\u0997\u09C7 \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\u09C7 \u098F\
  \u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0996\u09C1\u0981\u099C\u09C7\
  \ \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u0985\u09CD\u09AF\u09BE\u09AA\u09C7\
  \u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B0\u09BF\u09AE\u09BE\u0987\u09A8\u09CD\
  \u09A1\u09BE\u09B0, \u09B8\u09BE\u09AC\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\
  \u09B6\u09A8, \u09B6\u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982 \u098F\u09AC\u0982\
  \u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি এবং কেন?
ভবিষ্যত অথবা অতীতের একটি তারিখ হিসেব করা মানে নির্দিষ্ট সময়ের আগে অথবা পরে একটি তারিখ খুঁজে বের করা। অ্যাপের মধ্যে রিমাইন্ডার, সাবস্ক্রিপশন, শিডিউলিং এবং আরও অনেক সময়-ভিত্তিক ফিচারের জন্য প্রোগ্রামাররা এটি করে থাকেন।

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
