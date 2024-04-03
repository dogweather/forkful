---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:13:32.807917-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0987\u0996\u09BE\u09A8\
  \u09C7 PHP-\u09B0 \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\u0982\
  \u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3."
lastmod: '2024-03-17T18:47:44.116787-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0987\u0996\u09BE\u09A8\u09C7 PHP-\u09B0 \u09AC\u09BF\u09B2\u09CD\
  \u099F-\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
এইখানে PHP-র বিল্ট-ইন ফাংশন ব্যবহার করে একটি সরাসরি উদাহরণ:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // আউটপুট: Hello, she said, Its a fine day!
```

সহজ, তাই না? এই `str_replace()` ফাংশনটি স্ট্রিং থেকে অপসারণের জন্য চরিত্রগুলির একটি অ্যারে নেয়, এতে একক এবং ডাবল উদ্ধৃতি উভয়ই অন্তর্ভুক্ত থাকে।

## গভীর ডুব
PHP-র শুরুর দিনগুলির মধ্যে, বিকাশকারীদের স্ট্রিং অন্তর্ভুক্ত উদ্ধৃতিগুলি বিশেষ করে যখন ডাটাবেসে ডেটা ঢোকানো হতো, সেগুলির সাথে বাড়তি সতর্ক থাকতে হত। অনুচিতভাবে হ্যান্ডেল করা উদ্ধৃতি একে SQL ইনজেকশন হামলার দিকে নিয়ে যেতে পারে। ম্যাজিক উদ্ধৃতিগুলি, একটি বৈশিষ্ট্য যা ইনপুট ডেটা স্বয়ংক্রিয় পলায়ন করে। এটি খারাপ কোডিং অনুশীলন এবং নিরাপত্তা সমস্যা উৎসাহিত করায় অব্যাহত এবং অবশেষে সরিয়ে ফেলা হয়।

এখন, আমরা `str_replace()` বা আরো উন্নত প্যাটার্নের জন্য `preg_replace()` সহ regex ব্যবহার করি। এখানে একটি regex উদাহরণ:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

JSON ডেটার জন্য, আপনি উদ্ধৃতি পলায়নের জন্য অতিরিক্ত ব্যাকস্ল্যাশ এড়াতে `json_encode()` ফাংশনের সাথে `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` মত অপশনগুলি ব্যবহার করতে পারেন।

বাস্তবায়নের সময়, সীমান্ত ক্ষেত্রগুলি বিবেচনা করুন। যদি আপনার স্ট্রিংটি কিছু নির্দিষ্ট উদ্ধৃতি থাকতে চায়, যেমন একটি গল্পের সংলাপ বা মাপের ইঞ্চিতে? প্রসঙ্গ গুরুত্বপূর্ণ, তাই ডেটার উদ্দেশ্যমূলক ব্যবহারের সাথে আপনার উদ্ধৃতি-অপসারণ কাজটি পরিমার্জন করুন।

## দেখুন এছাড়াও
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL Injection প্রতিরোধ](https://owasp.org/www-community/attacks/SQL_Injection)
