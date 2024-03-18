---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:58.113601-06:00
description: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u0997\u09CB\u09B2 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\
  \u09B2\u09A4\u09BE\u09A4\u09C7 \u09A6\u09B6\u09AE\u09BF\u0995\u09C7\u09B0 \u0985\
  \u0982\u09B6\u0997\u09C1\u09B2\u09BF \u0995\u09C7\u099F\u09C7 \u09AB\u09C7\u09B2\
  \u09BE, \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u09AA\u09C2\u09B0\u09CD\
  \u09A3 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0997\u09A3\u09A8\u09BE\
  \ \u09B8\u09B9\u099C \u0995\u09B0\u09A4\u09C7, \u0995\u09BE\u09B0\u09CD\u09AF\u0995\
  \u09CD\u09B7\u09AE\u09A4\u09BE \u0989\u09A8\u09CD\u09A8\u09A4\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.124146-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u0997\u09CB\u09B2 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\
  \u09B2\u09A4\u09BE\u09A4\u09C7 \u09A6\u09B6\u09AE\u09BF\u0995\u09C7\u09B0 \u0985\
  \u0982\u09B6\u0997\u09C1\u09B2\u09BF \u0995\u09C7\u099F\u09C7 \u09AB\u09C7\u09B2\
  \u09BE, \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u09AA\u09C2\u09B0\u09CD\
  \u09A3 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0997\u09A3\u09A8\u09BE\
  \ \u09B8\u09B9\u099C \u0995\u09B0\u09A4\u09C7, \u0995\u09BE\u09B0\u09CD\u09AF\u0995\
  \u09CD\u09B7\u09AE\u09A4\u09BE \u0989\u09A8\u09CD\u09A8\u09A4\u09BF\u2026"
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
---

{{< edit_this_page >}}

## কি এবং কেন?
সংখ্যাগুলিকে গোল করা মানে এক নির্দিষ্ট নির্ভুলতাতে দশমিকের অংশগুলি কেটে ফেলা, প্রায়শই পূর্ণ সংখ্যায়। প্রোগ্রামাররা গণনা সহজ করতে, কার্যক্ষমতা উন্নতি করতে, বা আউটপুটগুলিকে ব্যবহারকারী-বান্ধব করতে সংখ্যাগুলিকে গোল করে।

## কিভাবে:
PHP সংখ্যাগুলিকে গোল করার কয়েকটি উপায় অফার করে: `round()`, `ceil()`, এবং `floor()`. এখানে কীভাবে তারা কাজ করে:

```php
echo round(3.14159);   // 3 ফিরিয়ে দেয়
echo round(3.14159, 2); // 3.14 ফিরিয়ে দেয়

echo ceil(3.14159);    // 4 ফিরিয়ে দেয়, সবসময় উপরে গোল করে

echo floor(3.14159);   // 3 ফিরিয়ে দেয়, সবসময় নিচে গোল করে
```

## গভীরে ডাইভ
অনুপযুক্তি অসীম দশমিকের সাথে মোকাবেলা করার ক্ষেত্রে গণিত এবং গণনার প্রাচীন কাল থেকে সংখ্যাগুলি গোল করা অপরিহার্য হয়ে উঠেছে। PHP তে, `round()` একটি নির্ভুলতা প্যারামিটার এবং মোড নিতে পারে, যা এর আচরণকে প্রভাবিত করে - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, ইত্যাদি এটি একটি ".5" সিনারিও মোকাবিলা করার সময় কিভাবে আচরণ করবে তা নির্দিষ্ট করে। অর্থনৈতিক অ্যাপ্লিকেশনে নির্ভুলতা গুরুত্বপূর্ণ, যেখানে গোল করা আইনগতভাবে নিয়ন্ত্রিত হতে পারে, যা `round()` কোডে বাস্তবায়ন করার উপায়কে প্রভাবিত করে।

অন্তর্ভুক্ত ফাংশনের বিকল্পগুলি কাস্টম গোল করার পদ্ধতি অথবা BC Math ফাংশনগুলি অন্তর্ভুক্ত করে, যা অবৈয়ক্তিক নির্ভুলতা অংকগণিতের জন্য উপযোগী, এবং উপযোগী হতে পারে যেখানে বেশি নিয়ন্ত্রণের প্রয়োজন বা খুব বড় সংখ্যাগুলি নিয়ে কাজ করার সময় স্থানীয় নির্ভুলতা বিফল হতে পারে।

## আরও দেখুন
PHP ম্যানুয়ালে আরও অন্বেষণ করুন:
- [PHP `round` ফাংশন](https://php.net/manual/en/function.round.php)
- [PHP `ceil` ফাংশন](https://php.net/manual/en/function.ceil.php)
- [PHP `floor` ফাংশন](https://php.net/manual/en/function.floor.php)
- [অবৈয়ক্তিক নির্ভুলতা অংকগণিতের জন্য BC Math](https://php.net/manual/en/book.bc.php)