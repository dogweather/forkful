---
title:                "স্ট্রিংকে লোয়ার কেসে রূপান্তর করা"
date:                  2024-03-17T17:46:51.782937-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

PHP-তে, স্ট্রিংকে লোয়ার কেসে রূপান্তর করা মানে একটি স্ট্রিংয়ের সমস্ত আলফাবেটিক অক্ষরকে তাদের লোয়ারকেস ভেরিয়েন্টে পরিবর্তন করা। প্রোগ্রামাররা এটি সাধারণত ধারাবাহিকতা অর্জনের জন্য করে, বিশেষ করে যখন স্ট্রিংগুলি তুলনা বা সাজানো হয়, যেখানে কেস-সেনসিটিভিটি জিনিসগুলি গোলমাল করে দিতে পারে।

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
