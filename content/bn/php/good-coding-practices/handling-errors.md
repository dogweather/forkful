---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:20.109317-06:00
description: "PHP \u09A4\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u098F\u09AE\u09A8 \u09AA\u09B0\u09BF\u09B8\u09CD\
  \u09A5\u09BF\u09A4\u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u098F\
  \u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\
  \ \u099C\u09BE\u09A8\u09BE\u09A8\u09CB \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\
  \u09C7 \u09AF\u09BE \u0995\u09B0\u09CD\u09AE\u09B8\u09C2\u099A\u09BF\u09B0 \u09B8\
  \u09CD\u09AC\u09BE\u09AD\u09BE\u09AC\u09BF\u0995 \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9\
  \ \u09AC\u09BF\u0998\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\
  \u09A8 \u0985\u09A8\u09C1\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4 \u09AB\u09BE\u0987\
  \u09B2 \u09AC\u09BE \u0996\u09BE\u09B0\u09BE\u09AA\u2026"
lastmod: '2024-03-17T18:47:44.138738-06:00'
model: gpt-4-0125-preview
summary: "PHP \u09A4\u09C7 \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u098F\u09AE\u09A8 \u09AA\u09B0\u09BF\u09B8\u09CD\
  \u09A5\u09BF\u09A4\u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u098F\
  \u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\
  \ \u099C\u09BE\u09A8\u09BE\u09A8\u09CB \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\
  \u09C7 \u09AF\u09BE \u0995\u09B0\u09CD\u09AE\u09B8\u09C2\u099A\u09BF\u09B0 \u09B8\
  \u09CD\u09AC\u09BE\u09AD\u09BE\u09AC\u09BF\u0995 \u09AA\u09CD\u09B0\u09AC\u09BE\u09B9\
  \ \u09AC\u09BF\u0998\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09C7\u09AE\
  \u09A8 \u0985\u09A8\u09C1\u09AA\u09B8\u09CD\u09A5\u09BF\u09A4 \u09AB\u09BE\u0987\
  \u09B2 \u09AC\u09BE \u0996\u09BE\u09B0\u09BE\u09AA\u2026"
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কী এবং কেন?
PHP তে ত্রুটি পরিচালনা এমন পরিস্থিতি পরিচালনা এবং প্রতিক্রিয়া জানানো সম্পর্কে যা কর্মসূচির স্বাভাবিক প্রবাহ বিঘ্নিত করে, যেমন অনুপস্থিত ফাইল বা খারাপ ডেটা ইনপুট। প্রোগ্রামাররা ত্রুটি পরিচালনা করে ক্র্যাশ প্রতিরোধ এবং ব্যবহারকারীদের একটি মসৃণ অভিজ্ঞতা প্রদান করতে।

## কীভাবে:
PHP-তে, আপনি `try-catch` ব্লক ব্যবহার করে ত্রুটি পরিচালনা করতে পারেন, এবং কাস্টম ত্রুটি হ্যান্ডলার এবং ব্যতিক্রম দিয়ে প্রক্রিয়াটি কাস্টমাইজ করতে পারেন।

```php
// বেসিক try-catch উদাহরণ
try {
  // কিছু ঝুঁকিপূর্ণ কাজ করুন
  $file = fopen("nonexistentfile.txt", "r");
} catch (Exception $e) {
  // ত্রুটি পরিচালনা করুন
  echo "Error: " . $e->getMessage();
}

// একটি কাস্টম ত্রুটি হ্যান্ডলার সেট করা
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// ব্যতিক্রম ব্যবহার করা
class MyException extends Exception {}

try {
  // কিছু করুন এবং একটি কাস্টম ব্যতিক্রম নিক্ষেপ করুন
  throw new MyException("Custom error!");
} catch (MyException $e) {
  // কাস্টম ব্যতিক্রম পরিচালনা করুন
  echo $e->getMessage();
}

// নমুনা আউটপুট:
// Error: fopen(nonexistentfile.txt): failed to open stream: No such file or directory
// Custom error!
```

## গভীরে ডুব:
আগের দিনে, PHP ত্রুটিগুলো সম্পর্কে বেশি সতর্কবার্তা এবং নোটিশে ছিল যা স্ক্রিপ্ট নির্বাহন বন্ধ করে নি। ভাষাটি পরিপক্ক হওয়ার সাথে সাথে, এটি PHP 5-এ প্রবর্তিত Exception ক্লাসের মাধ্যমে আরও শক্তিশালী অবজেক্ট-ভিত্তিক ত্রুটি পরিচালনা গ্রহণ করে। পরে, PHP 7 ত্রুটি এবং ব্যতিক্রমের মধ্যে পার্থক্য প্রতিষ্ঠা করে এরর ক্লাসগুলির সাথে এগিয়ে আসে।

`try-catch` ব্লকগুলির আগে, PHP ত্রুটিগুলি পরিচালনার জন্য `set_error_handler()` ব্যবহার করত। `try-catch` আরও পরিস্কার, আধুনিক। কিন্তু কাস্টম ত্রুটি হ্যান্ডলারের এখনও একটি স্থান রয়েছে, বিশেষ করে পুরানো কোড বা যখন আপনার সাধারণত অ-ব্যতিক্রম ত্রুটিগুলি ধরতে হবে।

PHP 7+ এর `Throwable` ইন্টারফেস মানে এটি একটি Error বা Exception হোক, আপনি উভয়কেই ধরতে পারেন। এটি হাতে রাখা ভাল কারণ এখন আপনি ক্রিটিকাল রানটাইম ত্রুটিগুলি মিস করেন না, যা আগে অনুসরণ করা কঠিন ছিল।

PHP-র নির্মিত পদ্ধতির বাইরে অল্টারনেটিভগুলি include লাইব্রেরি এবং ফ্রেমওয়ার্ক যা নিজস্ব ত্রুটি পরিচালনা ব্যবস্থা নিয়ে আসে, ফাইলে ত্রুটি লগিং বা ব্যবহারকারী-বান্ধব ত্রুটি পৃষ্ঠা প্রদর্শনের মতো আরও বৈশিষ্ট্য অফার করে।

## আরও দেখুন
- PHP ডকুমেন্টেশনে ব্যতিক্রম সম্পর্কে অফিসিয়াল: https://www.php.net/manual/en/language.exceptions.php
- ত্রুটি রিপোর্টিং সম্পর্কে PHP দ্য রাইট ওয়ে: https://phptherightway.com/#error_reporting
- PHP ম্যানুয়াল ত্রুটি পরিচালনা সম্পর্কে: https://www.php.net/manual/en/book.errorfunc.php
