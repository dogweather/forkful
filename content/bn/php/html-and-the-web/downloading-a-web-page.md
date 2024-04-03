---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:55.896259-06:00
description: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\
  \u099C \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u0985\u09B0\
  \u09CD\u09A5 \u0985\u09A8\u09B2\u09BE\u0987\u09A8 \u09AC\u09BF\u09B7\u09AF\u09BC\
  \u09AC\u09B8\u09CD\u09A4\u09C1 \u0986\u09B9\u09B0\u09A3 \u0995\u09B0\u09BE \u09AF\
  \u09BE\u09A4\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u0985\u09AB\u09B2\
  \u09BE\u0987\u09A8\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09AC\u09BE\
  \ \u09AC\u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\
  \u09B0\u09CD\u09AF\u09BE\u09AA\u09BF\u0982, \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.128723-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE \u0985\u09B0\u09CD\
  \u09A5 \u0985\u09A8\u09B2\u09BE\u0987\u09A8 \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\
  \u09B8\u09CD\u09A4\u09C1 \u0986\u09B9\u09B0\u09A3 \u0995\u09B0\u09BE \u09AF\u09BE\
  \u09A4\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u0985\u09AB\u09B2\u09BE\
  \u0987\u09A8\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09AC\u09BE \u09AC\
  \u09BF\u09B6\u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\
  \u09CD\u09AF\u09BE\u09AA\u09BF\u0982, \u09A1\u09C7\u099F\u09BE \u09AC\u09BF\u09B6\
  \u09CD\u09B2\u09C7\u09B7\u09A3, \u0985\u09A5\u09AC\u09BE \u0993\u09AF\u09BC\u09C7\
  \u09AC \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u099F\
  \u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09AE\u09BF\u09A5\u09B8\u09CD\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\
  \u099F\u09BF \u0995\u09B0\u09C7\u0964."
title: "\u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC\u09AA\u09C7\u099C\
  \ \u09A1\u09BE\u0989\u09A8\u09B2\u09CB\u09A1 \u0995\u09B0\u09BE"
weight: 42
---

## কিভাবে:
PHP এর মাধ্যমে ওয়েব পেজ ডাউনলোড করা প্রচুর সোজা। এখানে `file_get_contents()` ব্যবহার করে একটি সহজ উদাহরণ:

```php
<?php
$url = "http://example.com";
$pageContent = file_get_contents($url);

if ($pageContent !== false) {
    echo "পেজ সফলভাবে ডাউনলোড হয়েছে।\n";
    // $pageContent এর সাথে কাজ করুন
} else {
    echo "পেজ ডাউনলোড করতে ব্যর্থ হয়েছে।\n";
}
?>
```

এবং যদি আপনি আরও নিয়ন্ত্রণ চান অথবা HTTP হেডারস, কুকিজ, এবং POST অনুরোধ সামলাতে চান, তাহলে আপনি `cURL` দিয়ে একটু বিশেষ কাজ করতে পারেন:

```php
<?php
$url = "http://example.com";
$ch = curl_init($url);

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$pageContent = curl_exec($ch);

if (curl_errno($ch)) {
    echo "ত্রুটি: " . curl_error($ch) . "\n";
} else {
    echo "পেজ সফলভাবে ডাউনলোড হয়েছে।\n";
    // $pageContent এর সাথে কাজ করুন
}

curl_close($ch);
?>
```

উদাহরণ আউটপুট হতে পারে:
```
পেজ সফলভাবে ডাউনলোড হয়েছে।
```

## গভীর ডুব
ওয়েব পেজ ডাউনলোড করা ওয়েবের সাথে প্রাচীনতম অনুশীলন গুলির একটি। প্রাথমিকভাবে, ওয়েব পৃষ্ঠাগুলির সাথে মিথস্ক্রিয়া করার জন্য আপনি `wget` অথবা `curl` এর মতো কমান্ড-লাইন টুলস ব্যবহার করতেন। তবে, PHP বিকশিত হওয়ার সাথে সাথে, ফাংশনগুলি স্ক্রিপ্টের মধ্যে এই কাজগুলি করা সম্ভব হয়ে উঠেছে।

আমরা তুলনা করি:

- `file_get_contents()`: সহজ কাজের জন্য ভালো কিন্তু উন্নত বৈশিষ্ট্যের অভাব। ঝামেলা ছাড়াই দ্রুত ধরে ফেলার জন্য ভালো।
- `cURL`: PHP তে ওয়েব অনুরোধের জন্য সুইস আর্মি নাইফ। প্রমাণীকরণ, কুকিজ, এবং হেডার সেট করার মতো জটিল পরিস্থিতিগুলি সামলায়। একটু ভারী, কিন্তু আপনার যখন অতিরিক্ত শক্তির প্রয়োজন হয় তখন উপস্থিত।

অন্তরালে, `file_get_contents()` একটি স্ট্যান্ডার্ড GET অনুরোধ পাঠায়। অর্থাৎ এটি অভিনয় করে ঠিক যেন আপনি একটি URL টাইপ করার সময় একটি ব্রাউজারের মতো। কিন্তু HTTP প্রসঙ্গ (যেমন হেডার) ছাড়া, কিছু পেজ সঠিক বিষয়বস্তু ফেরত দিতে পারে না।

অন্যদিকে, `cURL` একটি ব্রাউজারের আচরণকে নিখুঁতভাবে অনুকরণ করতে পারে। তা নির্দিষ্ট হেডার বা কুকিজ প্রত্যাশিত চুলবুলে পেজগুলির জন্য আবশ্যক।

মনে রাখবেন, কিছু সাইট তাদের স্ক্র্যাপ করা পছন্দ করে না। সর্বদা `robots.txt` এবং সেবা শর্তাবলী শ্রদ্ধা করুন।

## দেখুন এছাড়াও
- [file_get_contents() এ PHP ম্যানুয়াল](http://php.net/manual/en/function.file-get-contents.php)
- [cURL এ PHP ম্যানুয়া�л](http://php.net/manual/en/book.curl.php)
- [robots.txt বিনির্দেশনা](https://developers.google.com/search/docs/advanced/robots/robots_txt)
