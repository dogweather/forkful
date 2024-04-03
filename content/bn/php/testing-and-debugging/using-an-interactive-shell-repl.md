---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:44.089482-06:00
description: "\u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09BE\u0995\
  \u09CD\u099F\u09BF\u09AD \u09B6\u09C7\u09B2 \u09AC\u09BE REPL (Read-Eval-Print Loop)\
  \ \u0986\u09AA\u09A8\u09BE\u0995\u09C7 PHP \u0995\u09CB\u09A1 \u09B2\u09BF\u0996\
  \u09A4\u09C7 \u098F\u09AC\u0982 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09BE\u09A5\u09C7\
  \ \u099A\u09BE\u09B2\u09BE\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\
  \u09BF \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u09A1\u09BF\u09AC\u09BE\u0997\
  \u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u09B6\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0986\u09A6\u09B0\u09CD\u09B6, \u0995\u09BE\u09B0\u09A3 \u0986\u09AA\
  \u09A8\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.131713-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09BE\u0995\
  \u09CD\u099F\u09BF\u09AD \u09B6\u09C7\u09B2 \u09AC\u09BE REPL (Read-Eval-Print Loop)\
  \ \u0986\u09AA\u09A8\u09BE\u0995\u09C7 PHP \u0995\u09CB\u09A1 \u09B2\u09BF\u0996\
  \u09A4\u09C7 \u098F\u09AC\u0982 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09BE\u09A5\u09C7\
  \ \u099A\u09BE\u09B2\u09BE\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u099F\
  \u09BF \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u09A1\u09BF\u09AC\u09BE\u0997\
  \u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u09B6\u09C7\u0996\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0986\u09A6\u09B0\u09CD\u09B6, \u0995\u09BE\u09B0\u09A3 \u0986\u09AA\
  \u09A8\u09BF \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F \u09A4\u09C8\u09B0\u09BF\u09B0 \u0985\u09A4\u09BF\u09B0\u09BF\
  \u0995\u09CD\u09A4 \u099D\u09BE\u09AE\u09C7\u09B2\u09BE \u099B\u09BE\u09A1\u09BC\
  \u09BE\u0987 \u0995\u09CB\u09A1\u09C7\u09B0 \u0996\u09A3\u09CD\u09A1 \u09AA\u09B0\
  \u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\
  \u09A8\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
টার্মিনালে `php -a` চালিয়ে PHP REPL চালু করুন। এটি কিভাবে কাজ করে তার একটি উদাহরণ এখানে:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
অ্যারে
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

আপনি ফাংশন নির্ধারণ করতেও পারেন:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## গভীর ভাবনা
REPL গুলি 1960 এর দশকে লিস্পের প্রাথমিক দিনগুলিতে কিছু আকারে প্রচলিত ছিল। PHP-র ইন্টারাক্টিভ শেল Python বা JavaScript এর মতো ভাষাগুলির তুলনায় কম উন্নত। এটি সেশনের মধ্যে রাষ্ট্র বজায় রাখে না এবং অটো-কমপ্লিশনের মতো বৈশিষ্ট্য অভাব রয়েছে। আরও বৈশিষ্ট্যসম্পন্ন PHP REPL এর জন্য, `psysh` বা `boris` এর মতো বিকল্পগুলি বিবেচনা করুন। এই তৃতীয় পক্ষের শেলগুলি ভাল অন্তর্দৃষ্টি টুল, ট্যাব-সম্পন্ন, এবং এমনকি একটি ডিবাগার অফার করে।

অন্তর্নিহিত, PHP-র REPL প্রতিটি লাইনের কোড কম্পাইল করে এবং এটি প্রবেশ করা মাত্র কার্যকর করে। একই সেশনে ক্লাস পুনঃনির্ধারণের মতো জিনিসের সাথে এই পদ্ধতির সীমাবদ্ধতা পরিষ্কার হয়ে ওঠে। এটি সরল পরীক্ষার জন্য দারুণ কিন্তু জটিল কাজের জন্য ঝামেলাসঙ্কুল হতে পারে।

## আরও দেখুন
- [PHP নির্দেশিকা - ইন্টারাক্টিভ শেল](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: PHP এর জন্য একটি রানটাইম ডেভেলপার কনসোল, ইন্টারাক্টিভ ডিবাগার এবং REPL](https://psysh.org/)
- [Boris: PHP-র জন্য একটি ছোট REPL](https://github.com/borisrepl/boris)
