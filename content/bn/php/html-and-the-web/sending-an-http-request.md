---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:22.164114-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP \u098F\u09B0 `cURL` \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\
  \u09AE\u09C7 HTTP \u09B0\u09BF\u0995\u09C1\u09AF\u09BC\u09C7\u09B8\u09CD\u099F \u09B9\
  \u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09C7\u09B2 \u0995\u09B0\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF \u099A\u09AE\u09CE\u0995\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u09A4\u09AC\u09C7 \u09A8\u09A4\
  \u09C1\u09A8 \u09AA\u09CD\u09B0\u099C\u09A8\u09CD\u09AE\u09C7\u09B0 \u098F\u0995\
  \u099F\u09BF \u0989\u09AA\u09BE\u09AF\u09BC \u09B9\u09B2 `file_get_contents`\u2026"
lastmod: '2024-03-17T18:47:44.126246-06:00'
model: gpt-4-0125-preview
summary: "PHP \u098F\u09B0 `cURL` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 HTTP \u09B0\u09BF\u0995\u09C1\
  \u09AF\u09BC\u09C7\u09B8\u09CD\u099F \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09C7\
  \u09B2 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u099A\u09AE\u09CE\u0995\
  \u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\
  \u0964 \u09A4\u09AC\u09C7 \u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u099C\u09A8\
  \u09CD\u09AE\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u09B9\u09B2 `file_get_contents` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE \u09B8\u09B9\u099C GET \u09B0\u09BF\u0995\u09C1\u09AF\u09BC\u09C7\u09B8\
  \u09CD\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF, \u0985\u09A5\u09AC\u09BE `stream_context_create`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE POST \u09B0\u09BF\
  \u0995\u09C1\u09AF\u09BC\u09C7\u09B8\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09A6\u09C1\u099F\u09BF\u09B0 \u098F\u0995\
  \u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u09AA\u09B0\u09BF\u099A\u09AF\u09BC\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\u0964\n\n#."
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
weight: 44
---

## কিভাবে:
PHP এর `cURL` লাইব্রেরির মাধ্যমে HTTP রিকুয়েস্ট হ্যান্ডেল করার একটি চমৎকার উপায় রয়েছে। তবে নতুন প্রজন্মের একটি উপায় হল `file_get_contents` ব্যবহার করা সহজ GET রিকুয়েস্টের জন্য, অথবা `stream_context_create` ব্যবহার করা POST রিকুয়েস্টের জন্য। এখানে দুটির একটি দ্রুত পরিচয় দেওয়া হলো।

### GET রিকুয়েস্টের সাথে file_get_contents():
```php
// যে URL এ হিট করছেন
$url = "http://example.com/api";

// একটি GET রিকুয়েস্ট পারফর্ম করার জন্য file_get_contents ব্যবহার করুন
$response = file_get_contents($url);

// আপনি কি পেয়েছেন দেখতে আউটপুট ডাম্প করুন
var_dump($response);
```

### POST রিকুয়েস্টের সাথে stream_context_create():
```php
// যে URL এ পোস্ট করছেন
$url = "http://example.com/api";

// যে ডেটা আপনি পাঠাচ্ছেন
$data = http_build_query([
    'foo' => 'bar',
    'baz' => 'qux',
]);

// স্ট্রিম কনটেক্সট অপশন
$options = [
    'http' => [
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => $data,
    ],
];

// একটি স্ট্রিম কনটেক্সট তৈরি করুন
$context  = stream_context_create($options);

// POST রিকুয়েস্ট পারফর্ম করুন এবং প্রতিক্রিয়াটি একটি ভেরিয়েবলে রাখুন
$result = file_get_contents($url, false, $context);

// আপনি কি পেয়েছেন দেখুন
var_dump($result);
```

## গভীর ডুব
প্রাথমিক দিকে, PHP HTTP রিকুয়েস্টের জন্য `fsockopen()` ছিল জনপ্রিয়। এটি একটু জটিল ছিল, তবে কাজ সম্পন্ন করতে পারত। তারপর `cURL` আগমন ঘটে, এটি এখনও শক্তিশালী এবং ব্যাপকভাবে ব্যবহৃত, বিশেষ করে জটিল অপারেশনের জন্য। তবে মাঝে মাঝে, আপনার শুধুমাত্র একটি স্ট্রিং কাটার জন্য চেইনস প্রয়োজন হয় না। সেই ক্ষেত্রে `file_get_contents()` এবং `stream_context_create()` উজ্জ্বল হয়ে ওঠে।

`file_get_contents()` সম্পর্কে একটি মূল বিষয় হল এর সহজতা। সাধারণ GET রিকুয়েস্টের জন্য এটি আদর্শ। কিন্তু যদি আপনার POST ডেটা প্রয়োজন হয়? `stream_context_create()` এর প্রবেশ ঘটে। এই ছোট্ট মুক্তা আপনাকে হেডার, মেথড এবং আরও অনেকের সাথে আপনার HTTP রিকুয়েস্ট সুনিপুণভাবে সাজাতে দেয়।

অভ্যন্তরে, `file_get_contents()` এবং `stream_context_create()` PHP এর স্ট্রিম র‌্যাপারগুলি ব্যবহার করে। এগুলি `fsockopen()` দ্বারা হ্যান্ডেল করা নিম্ন-স্তরের সকেট অপারেশনগুলিকে প্রতিস্থাপন করে।

একটি দুর্বলতা? ত্রুটি হ্যান্ডলিং কঠিন হতে পারে। যদি কিছু ভুল হয়, এই ফাংশনগুলি `cURL` এর চেয়ে কম ক্ষমাশীল। যদি আপনার বিস্তারিত প্রতিক্রিয়ার তথ্য প্রয়োজন হয় অথবা জটিল HTTP কাজের মোকাবেলা করতে হয়, `cURL` এর সাথে থাকা বিবেচনা করুন।

## আরও দেখুন
- cURL এর অফিসিয়াল PHP ডক: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)
- PHP স্ট্রিম কন্টেক্সটস: [https://www.php.net/manual/en/context.php](https://www.php.net/manual/en/context.php)
- HTTP কন্টেক্সট অপশনস: [https://www.php.net/manual/en/context.http.php](https://www.php.net/manual/en/context.http.php)
