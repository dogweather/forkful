---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:55:20.906902-06:00
description: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\
  \ \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\
  \u09A5 \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u0995\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0989\u09A6\u09CD\
  \u09A6\u09C7\u09B6\u09CD\u09AF \u09B8\u09B9 \u09AA\u09C1\u09A8\u0983\u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0\u09AF\u09CB\u0997\u09CD\u09AF \u09AC\u09CD\u09B2\u0995\
  \u09C7 \u09AD\u09BE\u0997 \u0995\u09B0\u09C7 \u09A8\u09C7\u09AF\u09BC\u09BE\u0964\
  \ \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u099C\u09BF\u09A8\
  \u09BF\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0997\u09CB\u099B\u09BE\u09A8\u09CB\
  \ \u09B0\u09BE\u0996\u09A4\u09C7,\u2026"
lastmod: '2024-03-17T18:47:44.136244-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7 \u0986\
  \u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5\
  \ \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u0995\u09C7\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u0989\u09A6\u09CD\u09A6\
  \u09C7\u09B6\u09CD\u09AF \u09B8\u09B9 \u09AA\u09C1\u09A8\u0983\u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u09AF\u09CB\u0997\u09CD\u09AF \u09AC\u09CD\u09B2\u0995\u09C7\
  \ \u09AD\u09BE\u0997 \u0995\u09B0\u09C7 \u09A8\u09C7\u09AF\u09BC\u09BE\u0964 \u0986\
  \u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u099C\u09BF\u09A8\u09BF\
  \u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0997\u09CB\u099B\u09BE\u09A8\u09CB\
  \ \u09B0\u09BE\u0996\u09A4\u09C7,\u2026"
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB"
---

{{< edit_this_page >}}

## কি এবং কেন?
কোডকে ফাংশনে আয়োজন করার অর্থ হলো আপনার কোডকে নির্দিষ্ট উদ্দেশ্য সহ পুনঃব্যবহারযোগ্য ব্লকে ভাগ করে নেয়া। আমরা এটি করি জিনিসগুলিকে গোছানো রাখতে, পুনরাবৃত্তি এড়াতে এবং ডিবাগিংকে সহজ করতে।

## কিভাবে:
কল্পনা করুন আমাদের কাছে ব্যবহারকারীদের অভিবাদন জানানোর জন্য পুনরাবৃত্তি কোড রয়েছে। এর পরিবর্তে, আমরা এটিকে একটি ফাংশনে মোড়ানোর মতো `greet_user`:

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

আউটপুট:
```
Hello, Alice!
Hello, Bob!
```

এখন, আপনার কাছে একটি সুবিধাজনক টুল আছে যা আপনি প্রতি বার একই লাইনের কোড পুনরায় লিখতে চাইলে ব্যবহার করতে পারেন না।

## গভীর ডুব
ফাংশন প্রোগ্রামিংয়ের প্রাথমিক দিনগুলি থেকে '৫০ এর দশকে FORTRAN থেকে আছে। এগুলি গঠনমূলক প্রোগ্রামিং এর একটি মূল পাথর এবং মডিউলারিটি এবং আইসোলেশন সম্পর্কে। বিকল্প? ভাল, আপনি অবজেক্ট-ওরিয়েন্টেডে যেতে পারেন এবং ক্লাস এবং মেথড সম্বন্ধে কথা বলতে পারেন, যা ফাংশনগুলিকে একটি ফ্যান্সি স্যুট পড়ানো। PHP এর ক্ষেত্রে, বাস্তবায়নের বিস্তারিত অন্তর্ভুক্ত করে প্যারামিটারের জন্য ডিফল্ট মান নির্দিষ্ট করা, ইনপুটের জন্য টাইপ হিন্টিং, এবং একটি অ্যারে বা, PHP 7.1 থেকে, একটি তালিকা ব্যবহার করে একাধিক মান ফেরত দেয়া সম্ভব।

এখানে টাইপ ঘোষণা এবং ডিফল্ট মানের সাথে একটি আধুনিক মোড় রয়েছে:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 এরো ফাংশনের সাথে এসেছে যা একলাইনের ফাংশন লিখতে সাহায্য করে, যা প্রায়শই অ্যারে অপারেশনে ব্যবহৃত হয়:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

আউটপুট:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## আরো দেখুন
- [ফাংশন সম্পর্কে PHP ম্যানুয়াল](https://www.php.net/manual/en/functions.user-defined.php)
- [ফাংশন - PHP: সঠিক পথ](https://phptherightway.com/#functions)
- [PHP 7.4 এরো ফাংশন সম্পর্কে জানুন](https://stitcher.io/blog/short-closures-in-php)
