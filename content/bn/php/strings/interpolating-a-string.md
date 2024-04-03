---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:23.632148-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2\u09C7\u09B0 \u09AE\u09BE\
  \u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u0987\u09A8\u099C\
  \u09C7\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C7 \u09AD\
  \u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u09B8\u09A8\
  \u09CD\u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.114729-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2\u09C7\u09B0 \u09AE\u09BE\
  \u09A8\u0997\u09C1\u09B2\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u098F\u0995\
  \u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u0987\u09A8\u099C\
  \u09C7\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C7 \u09AD\
  \u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2\u0997\u09C1\u09B2\u09BF \u09B8\u09A8\
  \u09CD\u09A8\u09BF\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7, \u09AF\u09BE \u0995\u09CB\u09A1\u0995\u09C7 \u0986\u09B0\u0993 \u09AA\u09B0\
  \u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u098F\u09AC\u0982 \u09AA\u09BE\u09A0\u09AF\
  \u09CB\u0997\u09CD\u09AF \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
PHP-তে, আপনি ডাবল উদ্ধৃতি অথবা হিয়ারডক সিনট্যাক্স ব্যবহার করে স্ট্রিংগুলি ইন্টারপোলেট করতে পারেন:

```php
$name = "World";
echo "Hello, $name!"; // আউটপুট: Hello, World!

// আরও জটিল ভেরিয়েবলগুলির জন্য কার্লি ব্রেসিস ব্যবহার করা
$object = new stdClass();
$object->greeting = "Hello";
echo "{$object->greeting}, $name!"; // আউটপুট: Hello, World!

// মাল্টি-লাইন স্ট্রিংগুলির জন্যে হিয়ারডক সিনট্যাক্স
$heredoc = <<<EOT
This is a string that contains $name within it.
You can write as much as you want here.
EOT;
echo $heredoc; // আউটপুট: This is a string that contains World within it.
```

লক্ষ্য করুন: সিংগেল কোটস ইন্টারপোলেট হবে না:

```php
echo 'Hello, $name!'; // আউটপুট: Hello, $name!
```

## গভীরে ডুব:
ইন্টারপোলেশন চালু করার আগে, PHP-তে ডট অপারেটর (.) দিয়ে যুক্ত করাই ছিল পথ। যেমন:

```php
echo 'Hello, ' . $name . '!';
```

ইন্টারপোলেশন এই প্রক্রিয়াকে সরাসরি ভেরিয়েবলটিকে স্ট্রিং-এর মধ্যে পার্স করে সহজ করে দেয়।

স্ট্রিং ইন্টারপোলেশন PHP 4 থেকে চালু আছে, কিন্তু PHP 7 এর সাথে কার্লি ব্রেস্টগুলির মধ্যে জটিল এক্সপ্রেশনের ব্যবহার আরও নমনীয় হয়েছে। এই উন্নতিগুলির সাথে, PHP যেকোনো ভেরিয়েবল, অবজেক্ট প্রপার্টিস এবং অ্যারে উপাদানগুলি সহজেই একটি স্ট্রিং-এর মধ্যে এম্বেড করার ব্যাপারে সহজ করে দিয়েছে।

ইন্টারপোলেশনের বিকল্পগুলি যেমন `sprintf()` ফরম্যাটেড স্ট্রিংগুলির জন্য অথবা `implode()` অ্যারেগুলির জন্য ব্যবহার করা হয়। এগুলি মাঝে মাঝে স্ট্রিং ফরম্যাটিং এর উপর আরও নিয়ন্ত্রণ অফার করতে পারে, বিশেষ করে লোকালাইজেশন এবং জটিল কাঠামোর জন্য।

ইমপ্লিমেন্টেশনের দিক থেকে, PHP ডাবল উদ্ধৃতি অথবা হিয়ারডক সিনট্যাক্স এর মধ্যে স্ট্রিংগুলিতে ভেরিয়েবলগুলি খুঁজে বের করে এবং তাদের ভেরিয়েবলের মানের সাথে প্রতিস্থাপিত করে। পারসারটি সিঙ্গেল-কোটেড স্ট্রিংগুলিতে ডলার চিহ্ন ($) একটি সাধারণ অক্ষর হিসেবে বিবেচনা করে, তা উপেক্ষা করে।

## আরও দেখুন
- [PHP: Strings](http://php.net/manual/en/language.types.string.php) - স্ট্রিং সম্পর্কিত অফিসিয়াল PHP ডকুমেন্টেশন।
- [PHP: Heredoc সিনট্যাক্স](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - Heredoc এর বিস্তারিত ব্যাখ্যা সম্পর্কিত PHP ম্যানুয়ালের অনুচ্ছেদ।
- [PHP: স্ট্রিং অপারেটরস](https://www.php.net/manual/en/language.operators.string.php) - স্ট্রিং যোগ এবং ডট অপারেটর সম্পর্কে আরও তথ্য।
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php) - স্ট্রিং ফর্ম্যাটিং এর জন্য `sprintf()` ফাংশনের ডকুমেন্টেশন।
