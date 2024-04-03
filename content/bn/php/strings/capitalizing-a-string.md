---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.333624-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AC\u09A1\u09BC \u09B9\u09BE\
  \u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\
  \u09A4 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09AD\u09BF\
  \u09A8\u09CD\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09B8\u09B9\u099C\u09BE\u09A4\
  \u09AD\u09BE\u09AC\u09C7 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7\
  , \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF \u09AA\u09C3\u09A5\u0995 \u0989\u09A6\
  \u09CD\u09A6\u09C7\u09B6\u09CD\u09AF \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09A8\
  \ \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.111064-06:00'
model: gpt-4-0125-preview
summary: "PHP \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\
  \u09C7 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AB\u09BE\u0982\u09B6\
  \u09A8 \u09B8\u09B9\u099C\u09BE\u09A4\u09AD\u09BE\u09AC\u09C7 \u09B8\u09AE\u09B0\
  \u09CD\u09A5\u09A8 \u0995\u09B0\u09C7, \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF\
  \ \u09AA\u09C3\u09A5\u0995 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF \u09AA\
  \u09B0\u09BF\u09AC\u09C7\u09B6\u09A8 \u0995\u09B0\u09C7\u0964 \u0986\u09AA\u09A8\
  \u09BF \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8 \u09A4\u09BE\u09B0 \u0989\u09AA\u09B0 \u098F\u0995 \u09A8\u099C\u09B0\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
PHP স্ট্রিংগুলিকে বড় হাতের অক্ষরে পরিণত করার জন্য বিভিন্ন ফাংশন সহজাতভাবে সমর্থন করে, প্রতিটি পৃথক উদ্দেশ্য পরিবেশন করে। আপনি কিভাবে তাদের ব্যবহার করতে পারেন তার উপর এক নজর:

### একটি স্ট্রিংয়ের প্রথম অক্ষরটি বড় করা:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // আউটপুট দেখায়: Hello, world!
```

### প্রতিটি শব্দের প্রথম অক্ষরটি বড় করা:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // আউটপুট দেখায়: Hello, World!
```

### সমগ্র স্ট্রিংটিকে বড় হাতের অক্ষরে পরিবর্তন করা:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // আউটপুট দেখায়: HELLO, WORLD!
```

আরো বেশি সামঞ্জস্যতা অথবা তৃতীয়-পক্ষের সমাধানের প্রয়োজনে, লাইব্রেরিগুলি যেমন `mbstring` (মাল্টিবাইট স্ট্রিং-এর জন্য) ব্যবহৃত হতে পারে, বিশেষ করে যখন আন্তর্জাতিকীকরণের সাথে মোকাবেলা করা হয় যেখানে অক্ষরগুলি মৌলিক ASCII সেটের বাইরে প্রসারিত হতে পারে।

### mbstring ব্যবহার করে UTF-8 স্ট্রিংগুলিকে বড় করা:
আপনার PHP কনফিগারেশনে `mbstring` এক্সটেনশন সক্রিয় আছে এমনটি নিশ্চিত করুন, তারপর:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // আউটপুট দেখায়: Élégant
```

এই পদ্ধতিটি সেইসব স্ট্রিংগুলিকে যথাযথভাবে বড় হাতের অক্ষরে পরিণত করতে সাহায্য করে যা নন-ASCII অক্ষর ধারণ করে, বিভিন্ন ভাষার নুয়ন্সগুলিতে মানানসই করে।
