---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:08.820285-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: HTML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, PHP \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8 \u0985\u09A5\u09AC\
  \u09BE \u09B0\u09CB\u09AC\u09BE\u09B8\u09CD\u099F \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AF\u09C7\u09AE\u09A8 Simple HTML DOM Parser-\u098F\u09B0\
  \ \u0989\u09AA\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09A4\u09C7\
  \u2026"
lastmod: '2024-04-05T21:53:52.542523-06:00'
model: gpt-4-0125-preview
summary: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF, PHP \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\
  \u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8 \u0985\u09A5\u09AC\u09BE \u09B0\u09CB\u09AC\u09BE\u09B8\
  \u09CD\u099F \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AF\u09C7\
  \u09AE\u09A8 Simple HTML DOM Parser-\u098F\u09B0 \u0989\u09AA\u09B0 \u09A8\u09BF\
  \u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7, \u0986\u09AE\u09B0\u09BE PHP-\u09B0 `DOMDocument`\
  \ \u098F\u09AC\u0982 Simple HTML DOM Parser \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09AC\u09BF\u09B6\
  \u09CD\u09B2\u09C7\u09B7\u09A3 \u0995\u09B0\u09AC\u0964."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

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
