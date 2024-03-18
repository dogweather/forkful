---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:16:35.462998-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
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
