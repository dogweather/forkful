---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:35.462998-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u0993 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\
  \u09AA\u09A8 \u098F\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u09AC\u09BF\
  \u09B6\u09C7\u09B7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u09C7 \u0996\u09C1\u0981\u099C\
  \u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09B8\u09C7\u0997\
  \u09C1\u09B2\u09BF\u0995\u09C7 \u0985\u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AC\u09A6\u09B2\u09C7 \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.113647-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u0993 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\
  \u09AA\u09A8 \u098F\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u09AC\u09BF\
  \u09B6\u09C7\u09B7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\
  \ \u0995\u09A8\u09CD\u099F\u09C7\u09A8\u09CD\u099F\u09C7 \u0996\u09C1\u0981\u099C\
  \u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE \u098F\u09AC\u0982 \u09B8\u09C7\u0997\
  \u09C1\u09B2\u09BF\u0995\u09C7 \u0985\u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AC\u09A6\u09B2\u09C7 \u09A6\u09C7\u0993\u09AF\
  \u09BC\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BF \u09A1\u09C7\u099F\u09BE\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
---

{{< edit_this_page >}}

## কি ও কেন?

টেক্সট অনুসন্ধান ও প্রতিস্থাপন এর মানে হলো বিশেষ স্ট্রিংগুলি কন্টেন্টে খুঁজে বের করা এবং সেগুলিকে অন্য কিছুর সাথে বদলে দেওয়া। প্রোগ্রামাররা এটি ডেটা আপডেট করতে, ত্রুটি সংশোধন করতে বা ম্যানুয়াল সম্পাদনা ছাড়াই টেক্সট পরিবর্তন করতে করে থাকেন।

## কিভাবে:

এখানে একটি দ্রুত উপায় দেওয়া হল যেখানে PHP ব্যবহার করে একটি বাক্যে 'cat' কে 'dog' এর সাথে প্রতিস্থাপন করা হল:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat';
$replacedText = str_replace('cat', 'dog', $text);

echo $replacedText;
?>
```

নমুনা আউটপুট:

```
The quick brown fox jumps over the lazy dog
```

এখন, ধরুন আমরা কেস-অনুবেদ্য প্রতিস্থাপনের সাথে মোকাবিলা করছি:

```PHP
<?php
$text = 'Catapults are CATegorically amazing!';
$replacedText = str_ireplace('cat', 'dog', $text);

echo $replacedText;
?>
```

নমুনা আউটপুট:

```
Dogapults are DOGegorically amazing!
```

## গভীর বিভাগ:

অনুসন্ধান ও প্রতিস্থাপন ফাংশন কম্পিউটিং এর প্রারম্ভিক দিন থেকেই ছিল — ইউনিক্স এর `sed` চিন্তা করুন। PHP তে, `str_replace` এবং `str_ireplace` হলো সহজ অনুসন্ধান ও প্রতিস্থাপনের জন্য আপনার বেছে নেওয়া ফাংশন। `str_replace` কেস-সংবেদনশীল, অন্যদিকে `str_ireplace` তা নয়।

এরা কিভাবে কাজ করে? অভ্যন্তরে, উভয় ফাংশনই স্ট্রিং এর প্রতিটি অংশ পরীক্ষা করে, মিল খুঁজে বের করে এবং তাদের প্রতিস্থাপন করে। এরা অ্যারেগুলিও হ্যান্ডেল করতে পারে, সুতরাং আপনি একসাথে একাধিক প্যাটার্ন অনুসন্ধান ও প্রতিস্থাপন করতে পারেন।

এখন, যদি আপনাকে আরও নিয়ন্ত্রণের প্রয়োজন হয়, যেমন প্যাটার্ন মিলানো, তাহলে আপনি `preg_replace` ব্যবহার করতে চাইবেন। এটি নিয়মিত এক্সপ্রেশনগুলি ব্যবহার করে, যা অনেক বেশি লচকতা এবং নির্ভুলতা অফার করে:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat 7 times.';
$replacedText = preg_replace('/\bcat\b/i', 'dog', $text);

echo $replacedText;
?>
```

নমুনা আউটপুট:

```
The quick brown fox jumps over the lazy dog 7 times.
```

এইটি 'cat' কে 'dog' এর সাথে প্রতিস্থাপন করেছে, কেস অবজ্ঞা করা হয়েছে (`/i` মডিফায়ার), এবং শুধুমাত্র সম্পূর্ণ শব্দগুলি মিলিত হয়েছে (`\b` শব্দ সীমা).

## আরও দেখুনঃ

- PHP অফিসিয়াল ডকুমেন্টেশন on str_replace: https://www.php.net/manual/en/function.str-replace.php
- PHP অফিসিয়াল ডকুমেন্টেশন on str_ireplace: https://www.php.net/manual/en/function.str-ireplace.php
- PHP অফিসিয়াল ডকুমেন্টেশন on preg_replace: https://www.php.net/manual/en/function.preg-replace.php
- নিয়মিত এক্সপ্রেশন টিউটোরিয়াল: https://www.regular-expressions.info/
- টেক্সট ফিল্টার ও পরিবর্তন করার জন্য Unix `sed` স্ট্রীম এডিটর: http://www.grymoire.com/Unix/Sed.html
