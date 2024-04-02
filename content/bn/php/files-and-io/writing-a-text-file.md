---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:44:27.469626-06:00
description: "PHP-\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE \u0985\u09A5\u09AC\u09BE \u0993\u09AA\u09C7\u09A8 \u0995\u09B0\u09C7\
  \ \u09A4\u09BE\u09A4\u09C7 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\
  \u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\u09A8\u09CB\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7\u09A8 \u09A1\u09C7\u099F\u09BE, \u09AF\u09C7\u09AE\u09A8 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u2026"
lastmod: '2024-03-17T18:47:44.151725-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09BE \u0985\u09A5\u09AC\u09BE \u0993\u09AA\u09C7\u09A8 \u0995\u09B0\u09C7\
  \ \u09A4\u09BE\u09A4\u09C7 \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F \u09AA\
  \u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\u09A8\u09CB\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09C7\u09A8 \u09A1\u09C7\u099F\u09BE, \u09AF\u09C7\u09AE\u09A8 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u2026"
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

## কি এবং কেন?
PHP-তে একটি টেক্সট ফাইল লেখা মানে একটি ফাইল তৈরি করা অথবা ওপেন করে তাতে কন্টেন্ট প্রবেশ করানো। প্রোগ্রামাররা এটি করেন ডেটা, যেমন ব্যবহারকারী তৈরি কন্টেন্ট অথবা লগ, প্রোগ্রামের লাইফসাইকেলের বাইরে সংরক্ষণ করার জন্য।

## কিভাবে:
PHP নিজে থেকে `file_put_contents`, `fopen` এর সাথে `fwrite`, এবং `fclose` মত ফাংশনগুলির মাধ্যমে ফাইল লেখার সমর্থন করে। এগুলি ব্যবহার করার উপায় নিচে দেওয়া হল:

### `file_put_contents` দ্বারা সহজ লেখা:
এই ফাংশনটি এক ধাপেই ফাইলে লেখা প্রক্রিয়াকে সহজ করে দেয়।
```php
$content = "Hello, world!";
file_put_contents("hello.txt", $content);
// যাচাই করে দেখা হয় যে ফাইল সফলভাবে লেখা হয়েছে কি না
if (file_exists("hello.txt")) {
    echo "ফাইল সফলভাবে তৈরি হয়েছে!";
} else {
    echo "ফাইল তৈরি করা সম্ভব হয়নি।";
}
```

### `fopen`, `fwrite`, এবং `fclose` দ্বারা উন্নত লেখা:
ফাইল লেখার উপরে আরও নিয়ন্ত্রণ, যেমন টেক্সট যোগ করা বা আরও ভুল সংশোধন, পেতে `fopen` কে `fwrite` এর সাথে ব্যবহার করুন।
```php
$file = fopen("hello.txt", "a"); // 'a' মোডে যোগ, 'w' মোডে লেখা
if ($file) {
    fwrite($file, "\nআরও কন্টেন্ট যোগ করা হচ্ছে।");
    fclose($file);
    echo "কন্টেন্ট সফলভাবে যোগ হয়েছে!";
} else {
    echo "ফাইল ওপেন করা সম্ভব হয়নি।";
}
```

#### আউটপুট হিসেবে ফাইল পড়া:
আমাদের কন্টেন্ট যাচাই করতে:
```php
echo file_get_contents("hello.txt");
```
**নমুনা আউটপুট:**
```
Hello, world!
আরও কন্টেন্ট যোগ করা হয়েছে।
```

### তৃতীয় পক্ষের লাইব্রেরি ব্যবহার:
আরও জটিল ফাইল অপারেশনের জন্য, `League\Flysystem` মত লাইব্রেরি ফাইল সিস্টেমের উপরে একটি আবস্ট্রাকশন লেয়ারের মাধ্যমে ব্যবহার করা যেতে পারে, কিন্তু PHP-র বিল্ট-ইন ফাংশনগুলি প্রায়শই মৌলিক ফাইল লেখা কাজের জন্য যথেষ্ট। যদি আপনি `Flysystem` অন্বেষণ করতে চান, তাহলে এখানে একটি সংক্ষিপ্ত উদাহরণ:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "এই ব্যবহার করে লেখা হয়েছে।");
```
এই উদাহরণটি ধরে নেয় যে আপনি কম্পোজার মাধ্যমে `league/flysystem` ইনস্টল করেছেন। তৃতীয় পক্ষের লাইব্রেরি বিভিন্ন স্টোরেজ সিস্টেমের সাথে নির্বিঘ্নে কাজ করার সময় আরও জটিল ফাইল হ্যান্ডলিং অনেক সহজ করে তোলে।
