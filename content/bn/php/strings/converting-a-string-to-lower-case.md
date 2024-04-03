---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:51.782937-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: PHP `strtolower` \u09AB\u09BE\u0982\
  \u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0\
  \ \u09B8\u09AE\u09B8\u09CD\u09A4 \u0985\u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u09B2\
  \u09CB\u09DF\u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4\
  \ \u0995\u09B0\u09C7\u0964 \u098F\u099F\u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09C7 \u09A4\u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\
  \u09C7\u0996\u09BE\u09A8\u09CB \u09B9\u09B2\u09CB."
lastmod: '2024-03-17T18:47:44.115757-06:00'
model: gpt-4-0125-preview
summary: "PHP `strtolower` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982\u09DF\u09C7\u09B0 \u09B8\u09AE\u09B8\u09CD\u09A4 \u0985\
  \u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u09B2\u09CB\u09DF\u09BE\u09B0 \u0995\u09C7\
  \u09B8\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09C7\u0964 \u098F\u099F\
  \u09BF \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\
  \ \u09A4\u09BE \u09A8\u09BF\u099A\u09C7 \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\
  \u09B2\u09CB."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কীভাবে:
PHP `strtolower` ফাংশন ব্যবহার করে একটি স্ট্রিংয়ের সমস্ত অক্ষরকে লোয়ার কেসে পরিণত করে। এটি কীভাবে কাজ করে তা নিচে দেখানো হলো:

```php
<?php
$originalString = "HeLLo WoRLD!";
$lowerCaseString = strtolower($originalString);

echo $lowerCaseString; // আউটপুট: hello world!
?>
```

যদি আপনি মাল্টিবাইট অক্ষর এনকোডিং, যেমন UTF-8, সামলাতে চান, তাহলে `mb_strtolower` ব্যবহার করুন:

```php
<?php
$originalString = "İstanbul";
$lowerCaseString = mb_strtolower($originalString, 'UTF-8');

echo $lowerCaseString; // আউটপুট: istanbul (İ-কে i তে সঠিকভাবে রূপান্তর করে)
?>
```

## গভীর বিশ্লেষণ
ঐতিহাসিকভাবে, PHP-র `strtolower` ফাংশনটি কেস রূপান্তরের জন্য গিয়েছে ফাংশন হিসেবে পরিচিত, যা PHP-র খুব প্রাথমিক সংস্করণগুলিতে পরিচিত হয়। তবে, PHP অ্যাপ্লিকেশনগুলি আরও বৈশ্বিক হওয়ার সাথে সাথে, মাল্টিবাইট অক্ষর এনকোডিংগুলি সঠিকভাবে হ্যান্ডেল করার প্রয়োজন `mb_strtolower` এর আগমন ঘটে।

`strtolower` এবং `mb_strtolower`-এর বিকল্পগুলি `mb_ereg_replace_callback` ফাংশন অথবা `preg_replace_callback` এর সাথে নিয়মিত এক্সপ্রেশন ব্যবহার করা অন্তর্ভুক্ত, তবে সাধারণ কেস রূপান্তরের জন্য এগুলি অতিরিক্ত।

PHP-তে, স্ট্রিংগুলি প্রথাগতভাবে বাইট-ভিত্তিক, চরিত্র-ভিত্তিক নয়, অর্থাৎ প্রতিটি বাইট একটি চরিত্র। এটি ASCII এর মত সিঙ্গেল-বাইট এনকোডিংয়ের জন্য কাজ করে, যেখানে প্রতিটি চরিত্র সত্যিই একটি বাইট। মাল্টিবাইট এনকোডিংয়ের জন্য, `mb_strtolower` চরিত্র এনকোডিং বুঝতে পারে এবং চরিত্রগুলিকে যেমন করা উচিত তেমনভাবে ব্যবহার করে।

## আরও দেখুন
- PHP ম্যানুয়ালে `strtolower`: https://www.php.net/manual/en/function.strtolower.php
- PHP ম্যানুয়ালে `mb_strtolower`: https://www.php.net/manual/en/function.mb-strtolower.php
- PHP ডেভেলপারদের জন্য UTF-8 এবং Unicode: https://www.php.net/manual/en/book.mbstring.php
