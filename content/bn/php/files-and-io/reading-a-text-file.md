---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:01.341071-06:00
description: "PHP-\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AC\u09BF\u09B7\
  \u09DF\u09AC\u09B8\u09CD\u09A4\u09C1 \u099F\u09C7\u09A8\u09C7 \u0986\u09A8\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u099C\u099F\u09BF\
  \u09B2 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09BE \u099B\u09BE\u09A1\u09BC\u09BE\
  \u0987 \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C, \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.150762-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\
  \u099F\u09C7 \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AC\u09BF\u09B7\
  \u09DF\u09AC\u09B8\u09CD\u09A4\u09C1 \u099F\u09C7\u09A8\u09C7 \u0986\u09A8\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u0995\u09CB\u09A1\u0995\u09C7 \u099C\u099F\u09BF\
  \u09B2 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09BE \u099B\u09BE\u09A1\u09BC\u09BE\
  \u0987 \u09A1\u09BE\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C, \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কি এবং কেন?
PHP-তে একটি টেক্সট ফাইল পড়া মানে আপনার স্ক্রিপ্টে ফাইল থেকে বিষয়বস্তু টেনে আনা। প্রোগ্রামাররা তাদের কোডকে জটিল করে তোলা ছাড়াই ডাটা স্টোরেজ, কনফিগারেশন অথবা বড় ডেটাসেট প্রক্রিয়া করতে এটি করে থাকেন।

## কিভাবে:
### `file_get_contents` ব্যবহার করে:
```PHP
$content = file_get_contents("example.txt");
echo $content;
```
নমুনা আউটপুট:
```
হ্যালো, ওয়ার্ল্ড!
এটি টেক্সট ফাইল থেকে বিষয়বস্তু।
```

### `fopen` এবং `fgets` ব্যবহার করে:
```PHP
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
```
নমুনা আউটপুট:
```
হ্যালো, ওয়ার্ল্ড!
এটি টেক্সট ফাইল থেকে বিষয়বস্তু।
```

### `file_put_contents` ব্যবহার করে ফাইলে লেখা:
```PHP
$newContent = "নতুন টেক্সট যোগ করুন।";
file_put_contents("example.txt", $newContent);
```

## গভীরে ডাইভ
টেক্সট ফাইল পড়া প্রোগ্রামিং যত পুরানো, তার প্রায় সমান। ডাটাবেজের আগে, কনফিগ ফাইল ও ব্যবহারকারীর ডাটা প্রায়ই সহজ টেক্সট ফাইলে থাকতো। XML এবং JSON ফাইলের মতো বিকল্পগুলো কাঠামোবদ্ধ, পার্স করা সহজ এবং জটিল ডাটা জন্য উপযুক্ত।

PHP-তে, `file_get_contents` এবং `file()` পড়ার জন্য দ্রুত; প্রথমটি সবকিছু একটি স্ট্রিং হিসাবে পেতে, এবং পরেরটি একটি অ্যারেতে। `fopen`-এর সঙ্গে `fgets` বা `fread` ব্যবহার করলে আপনি বেশি নিয়ন্ত্রণ পেতে পারেন, বিশেষ করে বড় ফাইলের ক্ষেত্রে, কারণ আপনি এটি লাইন বাই লাইন অথবা চাঙ্কে পড়তে পারেন।

কিছু নান্দনিকতা: `fopen` উপযুক্ত অনুমতি প্রয়োজন, নাহলে এটি ব্যর্থ হবে; এর ত্রুটিগুলি সম্ভালাটা সেরা অনুশীলন। `file_put_contents` ব্যবহার করার সময় মনে রাখবেন এটি ডিফল্টরুপে ফাইলটি ওভাররাইট করে; বিষয়বস্তু যোগ করার জন্য `FILE_APPEND` ফ্ল্যাগ ব্যবহার করুন।

## আরও দেখুন
- `file_get_contents` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.file-get-contents.php
- `fopen` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.fopen.php
- `fgets` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.fgets.php
- `file_put_contents` সম্পর্কে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.file-put-contents.php
- PHP ফাইল হ্যান্ডলিং সম্পর্কে টিউটোরিয়াল: https://www.w3schools.com/php/php_file.asp
