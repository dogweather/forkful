---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:27.300307-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP-\u09A4\u09C7, `date()` \u09AB\
  \u09BE\u0982\u09B6\u09A8\u099F\u09BF \u098F\u0995\u099F\u09BF \u099F\u09BE\u0987\
  \u09AE\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09AE\u09CD\u09AA\u0995\u09C7 \u098F\u0995\
  \u099F\u09BF \u0986\u09B0\u0993 \u09AA\u09A0\u09A8\u09C0\u09AF\u09BC \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \ \u0995\u09B0\u09C7\u0964 `DateTime` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\
  \u09C7\u09B0 \u09A4\u09BE\u09B0 `format()` \u09AE\u09C7\u09A5\u09A1 \u098F\u0995\
  \u0987 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF \u09AA\u09B0\u09BF\u09AC\
  \u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.143742-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7, `date()` \u09AB\u09BE\u0982\u09B6\u09A8\u099F\u09BF \u098F\
  \u0995\u099F\u09BF \u099F\u09BE\u0987\u09AE\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09AE\
  \u09CD\u09AA\u0995\u09C7 \u098F\u0995\u099F\u09BF \u0986\u09B0\u0993 \u09AA\u09A0\
  \u09A8\u09C0\u09AF\u09BC \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09AB\
  \u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u0995\u09B0\u09C7\u0964 `DateTime` \u0985\u09AC\
  \u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u09A4\u09BE\u09B0 `format()` \u09AE\u09C7\
  \u09A5\u09A1 \u098F\u0995\u0987 \u0989\u09A6\u09CD\u09A6\u09C7\u09B6\u09CD\u09AF\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0\u09C7\u09B0 \u09AA\u09CD\u09B0\u09BE\u0995\u099F\u09BF\u0995\u09BE\u09B2\
  \ \u0985\u09AC\u09B8\u09CD\u09A5\u09BE \u09A6\u09C7\u0996\u09BE\u09A8\u09CB \u09B9\
  \u09B2."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
PHP-তে, `date()` ফাংশনটি একটি টাইমস্ট্যাম্পকে একটি আরও পঠনীয় স্ট্রিং-এ ফরম্যাট করে। `DateTime` অবজেক্টের তার `format()` মেথড একই উদ্দেশ্য পরিবেশন করে। এখানে তাদের ব্যবহারের প্রাকটিকাল অবস্থা দেখানো হল:

```php
<?php
// date() function ব্যবহার করে
echo date('Y-m-d H:i:s') . "\n"; // আউটপুট: 2023-04-03 14:30:00 (উদাহরণ)

// DateTime অবজেক্ট ব্যবহার করে
$dateTime = new DateTime();
echo $dateTime->format('Y-m-d H:i:s') . "\n"; // আউটপুট: অনুরূপ
?>
```
নমুনা আউটপুট কোড চালানো হলে তারিখ এবং সময় প্রতিফলিত করে।

## ডিপ ডাইভ
ইতিহাসে, PHP তারিখ এবং সময় হ্যান্ডলিংয়ে বিকশিত হয়েছে। প্রারম্ভিক PHP সংস্করণগুলির তারিখ পরিচালনা সম্পর্কে কম বৈশিষ্ট্য ছিল। PHP 5.2.0-এ চালু করা `DateTime` ক্লাসটি অবজেক্ট-ভিত্তিক হ্যান্ডলিং, টাইমজোন সমর্থনের সাথে সাথে আরো বহুমুখীতা প্রদান করে।

`date()` এবং `DateTime`-এর বিকল্পগুলি অন্তর্ভুক্ত:
- `strftime()` (লোকেল-সচেতন ফরম্যাটিং)
- `DateTimeImmutable` (`DateTime`-এর অপরিবর্তনীয় সংস্করণ)
- আরো জটিল প্রয়োজনগুলির জন্য `Carbon` এর মতো এক্সটেনশন ক্লাসগুলি

অন্তর্নিহিতভাবে, উভয় `date()` এবং `DateTime` নির্দিষ্ট না করা পর্যন্ত সার্ভারের টাইমজোন সেটিংসের উপর নির্ভর করে। `DateTimeZone` ক্লাস টাইমজোনগুলির সাথে ম্যানিপুলেশন করতে পারে।

## আরো দেখুন
- [PHP ম্যানুয়াল: তারিখ এবং সময় ফাংশনস](https://www.php.net/manual/en/book.datetime.php)
- [PHP The Right Way: তারিখ এবং সময়](https://phptherightway.com/#date_and_time)
- [Carbon: DateTime-এর জন্য একটি সহজ PHP API এক্সটেনশন](https://carbon.nesbot.com/)
