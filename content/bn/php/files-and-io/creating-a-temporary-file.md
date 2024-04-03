---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:25.852228-06:00
description: "PHP-\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\
  \u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09A0\u09BF\u0995 \u09AF\u09A5\u09C7\u09B7\u09CD\u099F\
  \ \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A5\u09BE\u0995\
  \u09AC\u09C7, \u09A4\u09BE\u09B0\u09AA\u09B0 \u09AA\u09C1\u09AB\u2014\u098F\u099F\
  \u09BF \u099A\u09B2\u09C7 \u09AF\u09BE\u09AC\u09C7\u0964 \u098F\u099F\u09BF \u0995\
  \u09C7\u09A8 \u0995\u09B0\u09A4\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.152871-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\
  \u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AF\u09BE \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09A0\u09BF\u0995 \u09AF\u09A5\u09C7\u09B7\u09CD\u099F\
  \ \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09A5\u09BE\u0995\
  \u09AC\u09C7, \u09A4\u09BE\u09B0\u09AA\u09B0 \u09AA\u09C1\u09AB\u2014\u098F\u099F\
  \u09BF \u099A\u09B2\u09C7 \u09AF\u09BE\u09AC\u09C7\u0964 \u098F\u099F\u09BF \u0995\
  \u09C7\u09A8 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
PHP আপনাকে `tmpfile()` ফাংশনের মাধ্যমে অস্থায়ী ফাইল তৈরি করতে সাহায্য করে, যা আপনার সিস্টেমের টেম্প ডিরেক্টরিতে একটি ফাইল আপনার জন্য তৈরি করে। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```PHP
<?php
$tempFile = tmpfile();
fwrite($tempFile, "Hello, temporary world!");
rewind($tempFile);

echo fread($tempFile, 1024); // ফাইলে আমরা যা লিখেছি তা পড়ুন 

fclose($tempFile); // অস্থায়ী ফাইলটি স্বয়ংক্রিয়ভাবে সরানো হয়ে যায়
?>
```

নমুনা আউটপুট:
```
Hello, temporary world!
```

আপনি `tempnam()` ব্যবহার করে এমন একটি ফাইল নাম পেতে পারেন যা আপনি নিজে পরিচালনা করতে পারেন:

```PHP
<?php
$tempFilePath = tempnam(sys_get_temp_dir(), 'Tux');
file_put_contents($tempFilePath, "Penguins are cool!");

echo file_get_contents($tempFilePath); // কন্টেন্ট পড়ুন 

unlink($tempFilePath); // আপনি কাজ শেষে ফাইলটি মুছে ফেলুন
?>
```

নমুনা আউটপুট:
```
Penguins are cool!
```

## গভীর ডাইভ
`tmpfile()` ফাংশন PHP-এর প্রাথমিক দিনগুলো থেকে রয়েছে। এটি আপনার হয়ে ফাইল তৈরি এবং পরিষ্কার করার দায়িত্ব নেয়, বিশেষ করে উপকারী হতে পারে যখন স্পর্শকাতর ডেটা চারপাশে রেখে দেওয়ার সম্ভাব্য নিরাপত্তা ঝুঁকি থাকে।

অন্যদিকে, `tempnam()` আপনাকে শুধুমাত্র একটি নাম দেয়, ফাইল পরিচালনার দায়িত্ব আপনার হাতে ছেড়ে দেয়। একটি অসুবিধা: আপনি যখন কাজ শেষে, ফাইলটি `unlink()` করতে ভুলবেন না।

এই অস্থায়ী ফাইলগুলি সাধারণত আপনার সিস্টেমের ডিফল্ট টেম্প ডিরেক্টরিতে সংরক্ষিত হয়, যা আপনি `sys_get_temp_dir()` এর মাধ্যমে খুঁজে পেতে পারেন। এই অবস্থানটি আপনার অপারেটিং সিস্টেম এবং পরিবেশ কনফিগারেশনের উপর ভিত্তি করে ভিন্ন হতে পারে।

আপনার কাছে `tempnam()` এবং `tmpfile()` এর মতো বিকল্পও রয়েছে, এবং এই গোপন টেম্প ডিরেক্টরি পেতে `sys_get_temp_dir()` এর মতো আরও ফ্যান্সি অপশন রয়েছে। তবে মনে রাখবেন অস্থায়ী ফাইলের সাথে সোনালি নিয়ম: নিজেকে পরিষ্কার রাখুন—PHP এটি কিছুটা স্বয়ংক্রিয়ভাবে করে, তবে স্পষ্ট হওয়া ভালো অভ্যাস।

## আরও দেখুন
- [tmpfile() এর জন্য অফিসিয়াল PHP ডকুমেন্টেশন](https://www.php.net/manual/en/function.tmpfile.php)
- [tempnam() ফাংশনের উপর PHP ম্যানুয়াল](https://www.php.net/manual/en/function.tempnam.php)
- [sys_get_temp_dir() এর তথ্য PHP.net-এ](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [ফাইলসিস্টেম নিরাপত্তা](https://www.php.net/manual/en/security.filesystem.php)
