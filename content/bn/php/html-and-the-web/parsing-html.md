---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:08.820285-06:00
description: "PHP-\u09A4\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 HTML \u09A1\u0995\u09C1\u09AE\u09C7\u09A8\
  \u09CD\u099F\u09B8 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09A4\u09A5\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7\
  \ \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u09A1\u09C7\
  \u099F\u09BE \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09B6\u09A8\
  , \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u09AA\u09BF\u0982 \u0985\u099F\u09CB\u09AE\u09C7\u099F \u0995\u09B0\u09A4\u09C7\
  , \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.127231-06:00'
model: gpt-4-0125-preview
summary: "PHP-\u09A4\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 HTML \u09A1\u0995\u09C1\u09AE\u09C7\u09A8\
  \u09CD\u099F\u09B8 \u09A5\u09C7\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\
  \u09B7\u09CD\u099F \u09A4\u09A5\u09CD\u09AF \u09AC\u09C7\u09B0 \u0995\u09B0\u09C7\
  \ \u0986\u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u09A1\u09C7\
  \u099F\u09BE \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09BE\u0995\u09B6\u09A8\
  , \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09CD\u0995\u09CD\u09B0\u09CD\u09AF\u09BE\
  \u09AA\u09BF\u0982 \u0985\u099F\u09CB\u09AE\u09C7\u099F \u0995\u09B0\u09A4\u09C7\
  , \u0985\u09A5\u09AC\u09BE\u2026"
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কি এবং কেন?

PHP-তে HTML পার্স করা মানে হল HTML ডকুমেন্টস থেকে নির্দিষ্ট তথ্য বের করে আনা। প্রোগ্রামাররা এই কাজটি ডেটা এক্সট্রাকশন, ওয়েব স্ক্র্যাপিং অটোমেট করতে, অথবা তাদের এ্যাপ্লিকেশানের মধ্যে বিভিন্ন ওয়েব পেজ থেকে কন্টেন্ট ইন্টিগ্রেট করার জন্য করে থাকেন, যা ম্যানুয়াল হস্তক্ষেপ ছাড়াই ফাংশনালিটি বাড়ায়।

## কিভাবে:

HTML পার্স করার জন্য, PHP প্রোগ্রামাররা বিল্ট-ইন ফাংশন ব্যবহার করতে পারেন অথবা রোবাস্ট লাইব্রেরি যেমন Simple HTML DOM Parser-এর উপর নির্ভর করতে পারেন। এখানে, আমরা PHP-র `DOMDocument` এবং Simple HTML DOM Parser ব্যবহার করে উদাহরণ বিশ্লেষণ করব।

### `DOMDocument` ব্যবহার করে:
PHP-র `DOMDocument` ক্লাসটি এর DOM এক্সটেনশনের একটি অংশ, যা HTML এবং XML ডকুমেন্টগুলো পার্স এবং ম্যানিপুলেট করার অনুমতি দেয়। এখানে একটি দ্রুত উদাহরণ দেয়া হল কিভাবে `DOMDocument` ব্যবহার করে একটি HTML ডকুমেন্টের সব ইমেজ খুঁজে পাওয়া যায়:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>স্যাম্পল পৃষ্ঠা</title>
</head>
<body>
    <img src="image1.jpg" alt="চিত্র 1">
    <img src="image2.jpg" alt="চিত্র 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

নমুনা আউটপুট:
```
image1.jpg
image2.jpg
```

### Simple HTML DOM Parser ব্যবহার করে:
আরও জটিল কাজ বা সহজ সিনট্যাক্সের জন্য, আপনি হয়ত থার্ড-পার্টি লাইব্রেরি ব্যবহারে আগ্রহী হতে পারেন। Simple HTML DOM Parser একটি জনপ্রিয় পছন্দ, যা jQuery-র মত ইন্টারফেস প্রদান করে HTML স্ট্রাকচার ন্যাভিগেট এবং ম্যানিপুলেট করার জন্য। এখানে দেখানো হল এটি কীভাবে ব্যবহার করা যায়:

প্রথমে, কম্পোজার ব্যবহার করে লাইব্রেরিটি ইনস্টল করুন:
```
composer require simple-html-dom/simple-html-dom
```

তারপর, উদাহরণ স্বরূপ, সব লিংক খুঁজে পাওয়ার জন্য HTML ম্যানিপুলেট করুন:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

এই কোড স্নিপেটটি 'http://www.example.com' এর HTML কনটেন্ট ফেচ করবে, পার্স করবে, এবং সব হাইপারলিংক প্রিন্ট করবে। মনে রাখবেন, 'http://www.example.com' কে আপনার পার্স করতে ইচ্ছুক URL-এ প্রতিস্থাপন করতে হবে।

এই পদ্ধতিগুলি ব্যবহার করে, PHP ডেভেলপাররা কার্যকরভাবে HTML কন্টেন্ট পার্স করতে, তাদের প্রয়োজনমত ডেটা এক্সট্রাকশন কাস্টমাইজ করতে, অথবা বহিরাগত ওয়েব কন্টেন্টকে তাদের প্রজেক্টে সহজে ইন্টিগ্রেট করতে পারে।
