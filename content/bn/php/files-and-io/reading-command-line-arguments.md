---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:37.495801-06:00
description: "PHP-\u09A4\u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\
  \u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AF\u0996\u09A8 \u0995\u09A8\
  \u09B8\u09CB\u09B2\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\
  \u09B0\u09BF\u09AA\u09CD\u099F \u099A\u09BE\u09B2\u09BE\u09A8\u09CB \u09B9\u09AF\
  \u09BC, \u09A4\u0996\u09A8 \u09AA\u09BE\u09B8 \u0995\u09B0\u09BE \u0987\u09A8\u09AA\
  \u09C1\u099F\u0997\u09C1\u09B2\u09BF \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09A4\u09BE\u09A6\
  \u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.148078-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\
  \u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u09AF\u0996\u09A8 \u0995\u09A8\
  \u09B8\u09CB\u09B2\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\
  \u09B0\u09BF\u09AA\u09CD\u099F \u099A\u09BE\u09B2\u09BE\u09A8\u09CB \u09B9\u09AF\
  \u09BC, \u09A4\u0996\u09A8 \u09AA\u09BE\u09B8 \u0995\u09B0\u09BE \u0987\u09A8\u09AA\
  \u09C1\u099F\u0997\u09C1\u09B2\u09BF \u09A8\u09C7\u0993\u09AF\u09BC\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u09A4\u09BE\u09A6\
  \u09C7\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u0997\u09C1\u09B2\
  \u09BF\u0995\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09BF\u09AD \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\u0997\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC \u098F\u09AE\u09A8 \u0995\u09B0\
  \u09C7 \u0997\u09A1\u09BC\u09C7 \u09A4\u09CB\u09B2\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF, \u09AE\u09BE\u09A8\u0997\u09C1\u09B2\u09BF \u09B9\u09BE\u09B0\u09CD\u09A1\
  -\u0995\u09CB\u09A1 \u0995\u09B0\u09BE \u099B\u09BE\u09A1\u09BC\u09BE\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে:
PHP কমান্ড লাইন আর্গুমেন্টগুলি সংগ্রহ করার জন্য একটি গ্লোবাল অ্যারে `$argv` ব্যবহার করে, যেখানে `$argv[0]` হল স্ক্রিপ্টের নাম। এখানে আপনি এটি কিভাবে ব্যবহার করবেন:

```php
<?php
// check if any arguments are passed
if ($argc > 1) {
    echo "হ্যালো, " . $argv[1] . "!\n";
} else {
    echo "হ্যালো, তুমি যে কেউ হও!\n";
}
?>
```

আপনি যদি এই স্ক্রিপ্টটিকে `sayhello.php` বলে ডাকেন এবং চালান `php sayhello.php পৃথিবী`, তাহলে আউটপুট হবে:

```
হ্যালো, পৃথিবী!
```

কোনো আর্গুমেন্ট না? তাহলে পাবেন:

```
হ্যালো, তুমি যে কেউ হও!
```

## গভীরে যাওয়া
ঐতিহাসিক দৃষ্টিকোণ থেকে, কমান্ড লাইন স্ক্রিপ্টগুলি সিস্টেম অটোমেশনের ভিত্তি হিসেবে ব্যবহৃত হয়েছে, জিইউআই প্রকাশের আগে থেকেই। ওয়েব ডেভেলপমেন্টের জন্য বেশি পরিচিত PHP, সিএলআই সাপোর্টের জন্যও দৃঢ় সমর্থন প্রদান করে।

PHP-তে আর্গুমেন্ট পড়ার দুটি প্রধান উপায় হল `$argv` এবং `getopt()` ফাংশন। আগেরটি একটি সাধারণ অ্যারে যেখানে `getopt()` এর মাধ্যমে অপশনগুলি (মানসহ বা মান ছাড়া) পার্স করার মতো আরও জটিল কার্যকারিতা প্রদান করা হয়।

বাস্তবায়নের ক্ষেত্রে, `$argv` এবং `$argc` (আর্গুমেন্ট গণনা) CLI মোডে স্বয়ংক্রিয়ভাবে উপলব্ধ — অতিরিক্ত সেটআপের প্রয়োজন নেই। ওয়েব স্ক্রিপ্ট চালানোর সময় এগুলো উপস্থিত নেই কারণ তা তাদের ক্ষেত্র নয়।

তবে মনে রাখবেন, যদি আপনি `php.ini` বা সার্ভার কনফিগারেশনের মাধ্যমে `argv` এবং `argc`-কে গ্লোবাল ভেরিয়েবল হিসেবে নিবন্ধন করেন, তাহলে তাদেরকে ওয়েব স্ক্রিপ্টগুলিতেও অ্যাক্সেস করা যায়। যদিও, এটি বিরল এবং একটি নিরাপত্তা ঝুঁকি হতে পারে।

## দেখুন
আরও জটিল কমান্ড লাইন পার্সিংয়ের জন্য:
- [PHP.net getopt](https://www.php.net/manual/en/function.getopt.php)

PHP-র CLI সার্ভারে ডাইভ করা:
- [PHP.net Command line usage](https://www.php.net/manual/en/features.commandline.php)

PHP কমিউনিটির সাথে যোগাযোগ:
- [PHP CLI আলোচনা স্ট্যাক ওভারফ্লোতে](https://stackoverflow.com/questions/tagged/php+cli)
