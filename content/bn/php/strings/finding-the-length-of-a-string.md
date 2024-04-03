---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:19.363791-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\
  \u09B0\u09CD\u0998\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u0995\u09A4\u0997\u09C1\
  \u09B2\u09BF \u0985\u0995\u09CD\u09B7\u09B0 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0997\
  \u09A0\u09BF\u09A4 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\
  \ \u0995\u09B0\u09BE\u0964 \u0987\u09A8\u09AA\u09C1\u099F \u09AC\u09C8\u09A7\u09A4\
  \u09BE \u09AF\u09BE\u099A\u09BE\u0987, \u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE, \u0985\u09A5\
  \u09AC\u09BE \u0995\u09C7\u09AC\u09B2 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AB\
  \u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0982 \u098F\u09B0 \u09AE\u09A4\
  \u2026"
lastmod: '2024-03-17T18:47:44.119865-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u0995\u09A4\u0997\u09C1\u09B2\
  \u09BF \u0985\u0995\u09CD\u09B7\u09B0 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0997\u09A0\
  \u09BF\u09A4 \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\
  \u09B0\u09BE\u0964 \u0987\u09A8\u09AA\u09C1\u099F \u09AC\u09C8\u09A7\u09A4\u09BE\
  \ \u09AF\u09BE\u099A\u09BE\u0987, \u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE, \u0985\u09A5\u09AC\
  \u09BE \u0995\u09C7\u09AC\u09B2 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AB\u09B0\
  \u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09BF\u0982 \u098F\u09B0 \u09AE\u09A4 \u0995\
  \u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09A6\u09C7\u09B0 \u09AA\u09CD\u09B0\u09BE\u09AF\
  \u09BC\u0987 \u098F\u0987 \u09A4\u09A5\u09CD\u09AF\u09C7\u09B0 \u09AA\u09CD\u09B0\
  \u09AF\u09BC\u09CB\u099C\u09A8 \u09B9\u09AF\u09BC\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কি এবং কেন?

স্ট্রিং এর দৈর্ঘ্য নির্ণয় করা মানে এটি কতগুলি অক্ষর নিয়ে গঠিত তা নির্ধারণ করা। ইনপুট বৈধতা যাচাই, সাবস্ট্রিং পরিচালনা, অথবা কেবল আউটপুট ফর্ম্যাটিং এর মত কাজের জন্য প্রোগ্রামারদের প্রায়ই এই তথ্যের প্রয়োজন হয়।

## কীভাবে করবেন:

`strlen()` ফাংশনটি নিম্নলিখিতভাবে ব্যবহার করুন:

```php
<?php
$text = "Hello, world!";
$length = strlen($text);
echo $length; // আউটপুট: 13
?>
```

এটি চালালে, আপনি আপনার পর্দায় `13` দেখতে পাবেন কারণ "Hello, world!" এর দৈর্ঘ্য ১৩ অক্ষর, স্পেস এবং বিস্ময়সূচক চিহ্নসহ।

## গভীরে ডুব:

`strlen()` ফাংশন PHP এর প্রারম্ভিক সংস্করণ থেকেই অংশ। এটি সরল এবং সাধারণত বাইটের সংখ্যার উপর ভিত্তি করে কাজ করে, যা বিশেষ এনকোডিং বিবেচনা ছাড়াই সাধারণত স্ট্রিংয়ের অক্ষরগুলির সংখ্যার সমতুল।

তবে, ওয়েব অ্যাপ্লিকেশনের আন্তর্জাতিককরণের সঙ্গে, একাধিক ভাষা এবং অক্ষর এনকোডিং নিয়ে কাজ করা সাধারণ হয়ে উঠেছে। যেমন UTF-8 এ অক্ষরগুলি একাধিক বাইট ব্যবহার করতে পারে। এটি এখানে `mb_strlen()` প্রযোজ্য:

```php
<?php
// মাল্টিবাইট অক্ষরগুলি সহ একটি স্ট্রিং
$multibyteText = "こんにちは";
$length = mb_strlen($multibyteText, "UTF-8");
echo $length; // আউটপুট: 5
?>
```

পাঁচ অক্ষর, তবে আরও বাইট। `mb_strlen()` ফাংশন অক্ষর এনকোডিংকে মেনে চলে, মাল্টিবাইট স্ট্রিংগুলির জন্য সঠিক দৈর্ঘ্য পরীক্ষার নিশ্চয়তা দেয়।

`strlen()` দ্রুত এবং এক-বাইট অক্ষর সেটের জন্য উপযুক্ত। `mb_strlen()`, আরও জটিল এনকোডিং সামাল দেওয়ার প্রয়োজনে কিছুটা ধীর, আন্তর্জাতিক টেক্সটের সঙ্গে কাজ করার সময় অপরিহার্য।

## আরও দেখুন

- [PHP `strlen()` অফিসিয়াল ডকুমেন্টেশন](https://www.php.net/manual/en/function.strlen.php)
- [PHP `mb_strlen()` অফিসিয়াল ডকুমেন্টেশন](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP মাল্টিবাইট স্ট্রিং এক্সটেনশন](https://www.php.net/manual/en/book.mbstring.php)
