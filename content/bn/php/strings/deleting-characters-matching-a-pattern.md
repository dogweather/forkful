---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:42.888220-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP `preg_replace` \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7 \u098F\u09AE\u09A8 \u0985\u0995\u09CD\
  \u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0985\u0999\u09CD\u0995\
  \u2026"
lastmod: '2024-03-17T18:47:44.112297-06:00'
model: gpt-4-0125-preview
summary: "PHP `preg_replace` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\u09A4\
  \ \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\
  \u09B0\u09CD\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09C7\u09B2\u09C7\
  \ \u098F\u09AE\u09A8 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\
  \u09C1\u099B\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\
  \u0995\u09C7 \u0985\u0999\u09CD\u0995 \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\u09A8\u09CB\
  \ \u09B9\u09B2."
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে:
PHP `preg_replace` ফাংশন ব্যবহার করে নিয়মিত এক্সপ্রেশন ব্যবহার করে প্যাটার্নের সাথে মেলে এমন অক্ষরগুলি মুছে দেয়। এখানে একটি স্ট্রিং থেকে অঙ্ক মুছে ফেলার উপায় দেখানো হল:

```PHP
<?php
$text = "Year 2023!";
$pattern = '/\d+/'; // সমস্ত অঙ্ক মেলানোর প্যাটার্ন
$result = preg_replace($pattern, '', $text);
echo $result; // আউটপুট: Year !
?>
```

এবং এখানে ফাঁক স্থান মুছে ফেলার উপায় দেখানো হল:

```PHP
<?php
$text = "Too   many      spaces!";
$pattern = '/\s+/'; // সমস্ত ফাঁক স্থান মেলানোর প্যাটার্ন
$result = preg_replace($pattern, ' ', $text);
echo $result; // আউটপুট: Too many spaces!
?>
```

## গভীর ডুব দেওয়া
প্যাটার্নের সাথে মিলে যাওয়া অক্ষরগুলি মুছে ফেলা নতুন কিছু নয়। PHP এর `preg_replace` ফাংশন, যা এই কার্যকারিতাকে চালু করে, Perl-সামঞ্জস্যপূর্ণ নিয়মিত এক্সপ্রেশন ব্যবহার করে, যা Perl এর উত্থানের শেষের দিকে থেকে পাঠ্য প্রক্রিয়াকরণের একটি মূল উপাদান। `preg_replace` এর বিকল্পগুলির মধ্যে রয়েছে `str_replace` সরল প্রতিস্থাপনের জন্য এবং স্ট্রিং থেকে ফাঁক স্থান মুছে ফেলার জন্য `trim`, `ltrim`, এবং `rtrim`। আরও জটিল প্যাটার্ন মুছে ফেলার জন্য, প্রতিস্থাপন প্রক্রিয়া করার সময় অতিরিক্ত নিয়ন্ত্রণের জন্য `preg_replace_callback` ব্যবহার করা যেতে পারে।

`preg_replace` এ PREG মানে Perl Regular Expressions, যা PHP এর Perl এর প্যাটার্ন সিনট্যাক্স ব্যবহার করার ইঙ্গিত দেয়। এখানে বিশ্লেষণ:

- `\d` যেকোনো অঙ্ক খুঁজে পায়। `+` যোগ করা মানে পূর্ববর্তী উপাদানের (এই ক্ষেত্রে অঙ্ক) এক বা একাধিক।
- `\s` যেকোনো ফাঁক স্থান খুঁজে পায়। সংখ্যার মতো, `\s` পরে `+` ব্যবহার করা দীর্ঘ ফাঁক স্থান লক্ষ্য করে।

`preg_replace` এবং এর বিকল্পগুলির মধ্যে নির্বাচন করতে কি করছেন তা উপর নির্ভর করে। জটিল প্যাটার্নের জন্য `preg_replace` ব্যবহার করুন এবং সরল, সরাসরি প্রতিস্থাপনের ক্ষেত্রে `str_replace` ব্যবহার করুন।

নিয়মিত এক্সপ্রেশনের অপব্যবহার অকার্যকর কোডের দিকে নিয়ে যেতে পারে। সর্বদা বেঞ্চমার্ক করুন এবং নিয়মিত এক্সপ্রেশনগুলি বুদ্ধিমানভাবে ব্যবহার করুন।

## আরও দেখুন
PHP এর স্ট্রিং ফাংশন এবং প্যাটার্ন ম্যাচিং সম্পর্কে আরও জানতে:
- [PHP ম্যানুয়াল — preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP ম্যানুয়াল — Regular Expressions (Perl-Compatible)](https://www.php.net/manual/en/book.pcre.php)
- [PHP ম্যানুয়াল — str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP ম্যানুয়া�
