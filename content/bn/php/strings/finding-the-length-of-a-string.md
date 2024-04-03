---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:19.363791-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8\
  : `strlen()` \u09AB\u09BE\u0982\u09B6\u09A8\u099F\u09BF \u09A8\u09BF\u09AE\u09CD\
  \u09A8\u09B2\u09BF\u0996\u09BF\u09A4\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8."
lastmod: '2024-03-17T18:47:44.119865-06:00'
model: gpt-4-0125-preview
summary: "`strlen()` \u09AB\u09BE\u0982\u09B6\u09A8\u099F\u09BF \u09A8\u09BF\u09AE\
  \u09CD\u09A8\u09B2\u09BF\u0996\u09BF\u09A4\u09AD\u09BE\u09AC\u09C7 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C1\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

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
