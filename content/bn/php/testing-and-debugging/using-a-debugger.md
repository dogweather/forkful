---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:02.436957-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP-\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\
  \u099F\u09BF\u09AD \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u0986\u099B\u09C7\
  \ \u09AF\u09BE\u0995\u09C7 \u09AC\u09B2\u09BE \u09B9\u09AF\u09BC Xdebug\u0964 \u098F\
  \u099F\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8 \u09A4\u09BE \u098F\u0996\u09BE\u09A8\
  \u09C7\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\
  \u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u09AF\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u0995\u09BE\u099B\u09C7 Xdebug \u0987\u09A8\u09CD\u09B8\u099F\u09B2\u2026"
lastmod: '2024-03-17T18:47:44.135048-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\u09BF \u0987\u09A8\
  \u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BF\u09AD \u09A1\u09BF\u09AC\u09BE\
  \u0997\u09BE\u09B0 \u0986\u099B\u09C7 \u09AF\u09BE\u0995\u09C7 \u09AC\u09B2\u09BE\
  \ \u09B9\u09AF\u09BC Xdebug\u0964 \u098F\u099F\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8\
  \ \u09A4\u09BE \u098F\u0996\u09BE\u09A8\u09C7\u0964\n\n\u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u09AF\
  \u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 Xdebug \u0987\u09A8\
  \u09CD\u09B8\u099F\u09B2 \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u0986\u099B\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 `php.ini`\
  \ \u09AB\u09BE\u0987\u09B2\u09C7."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
PHP-র সাথে একটি ইন্টারেক্টিভ ডিবাগার আছে যাকে বলা হয় Xdebug। এটি কিভাবে ব্যবহার করবেন তা এখানে।

প্রথমে, নিশ্চিত করুন যে আপনার কাছে Xdebug ইন্সটল এবং কনফিগার করা আছে আপনার `php.ini` ফাইলে:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

পরবর্তী, একটি সাধারণ PHP স্ক্রিপ্ট লিখুন যাতে একটি বাগ থাকে:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // ওহো! এটি একটি প্লাস হওয়া উচিত, মাইনাস নয়
}

$result = add(1, 2);
echo "Result is: $result"; // আউটপুট হওয়া উচিত 3, -1 নয়
```

PhpStorm এর মতো একটি IDE ব্যবহার করে, লাইন নম্বরের পাশে ক্লিক করে একটি ব্রেকপয়েন্ট সেট করুন। ডিবাগার চালান এবং দেখুন কিভাবে ভেরিয়েবলগুলি পরিবর্তন হয় যখন আপনি এক্সিকিউশনের মধ্য দিয়ে পা ফেলেন। যখন আপনি `add` ফাংশন উপর পা ফেলবেন, তখন আপনি লক্ষ্য করবেন যে `$result` হয়ে যায় -1, যা অপ্রত্যাশিত।

## গভীরে ডুব:
ঐতিহাসিকভাবে, PHP প্রধানত ছোট স্ক্রিপ্টের জন্য ব্যবহৃত হতো, এবং ডিবাগিং মানেই ছিল কোডের মধ্যে জুড়ে `var_dump()` এবং `print_r()` স্টেটমেন্ট যোগ করা। সময়ের সাথে সাথে, PHP ওয়েব ডেভেলপমেন্টে একটি প্রধান খেলোয়াড় হয়ে ওঠার সাথে সাথে, Xdebug এবং Zend Debugger এর মতো আরও উন্নত টুলগুলি ব্যবহারের মধ্যে আসে।

Xdebug এর বিকল্পের মধ্যে pcov এবং phpdbg রয়েছে। এগুলি বিভিন্ন বৈশিষ্ট্য সরবরাহ করে, তবে Xdebug এর মতো পূর্ণাঙ্গ নাও হতে পারে। phpdbg একটি হালকা, PHP-নির্দিষ্ট ডিবাগার যা PHP 5.6 এর সাথে বিতরণ করা হয়, এবং pcov একটি কোড কভারেজ ড্রাইভার।

একটি ডিবাগার বাস্তবায়ন করার সময় মনে রাখবেন যে আপনি কখনো আপনার উৎপাদন সার্ভারে ডিবাগার চালু রাখতে পারবেন না, কারণ এটি নিরাপত্তা দুর্বলতা প্রকাশ করতে পারে এবং পারফরম্যান্স ধীর করে দিতে পারে।

## আরও দেখুন:
- [Xdebug ডকুমেন্টেশন](https://xdebug.org/docs/)
- [PhpStorm ডিবাগিং গাইড](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net-এ phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [GitHub-এ pcov](https://github.com/krakjoe/pcov)
