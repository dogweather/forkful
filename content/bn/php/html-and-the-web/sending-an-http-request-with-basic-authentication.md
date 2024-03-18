---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:39.414460-06:00
description: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\
  \u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 HTTP \u0985\u09A8\u09C1\
  \u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u098F\u09B0 \u0985\u09B0\
  \u09CD\u09A5 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\
  \u09C7 \u09B0\u09BF\u09B8\u09CB\u09B0\u09CD\u09B8 \u098F\u0995\u09CD\u09B8\u09C7\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0 \u09A8\u09BE\
  \u09AE \u0993 \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AF\
  \u09CB\u0997 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.129732-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\
  \u0995\u09C7\u09B6\u09A8\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 HTTP \u0985\u09A8\u09C1\
  \u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB \u098F\u09B0 \u0985\u09B0\
  \u09CD\u09A5 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\u09B0\
  \u09C7 \u09B0\u09BF\u09B8\u09CB\u09B0\u09CD\u09B8 \u098F\u0995\u09CD\u09B8\u09C7\
  \u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0 \u09A8\u09BE\
  \u09AE \u0993 \u09AA\u09BE\u09B8\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u09AF\
  \u09CB\u0997 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
---

{{< edit_this_page >}}

## কি ও কেন?

বেসিক অথেন্টিকেশনের সাথে HTTP অনুরোধ পাঠানো এর অর্থ একটি সার্ভারে রিসোর্স এক্সেস করার জন্য একটি ব্যবহারকারী নাম ও পাসওয়ার্ড যোগ করা। প্রোগ্রামাররা এটি ব্যবহার করে কারণ কিছু API এবং ওয়েব সার্ভিস অনুমোদিত ব্যবহারকারীরা তাদের ডাটা এক্সেস করার নিশ্চয়তার জন্য অথেন্টিকেশন চায়।

## কিভাবে:

এখানে PHP এর সাহায্যে cURL ব্যবহার করে বেসিক অথেন্টিকেশনের সাথে একটি HTTP অনুরোধ পাঠানোর সহজ উপায় তুলে ধরা হল:

```PHP
<?php
$url = 'https://api.example.com/data';
$username = 'your_username';
$password = 'your_password';

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

নমুনা আউটপুট:

``` 
{
  "authenticated": true,
  "data": "কিছু সুরক্ষিত ডাটা"
}
```

## গভীরে যাওয়া

HTTP বেসিক অথেন্টিকেশন ওয়েবের প্রারম্ভিক দিনগুলি থেকে ব্যবহৃত হয়ে আসছে। এটি সেইরকম নিরাপদ অপশন নয় (কারণ ক্রেডেনশিয়ালগুলি বেস৬৪ এনকোডিংয়ে পাঠানো হয়, যা সহজেই ডিকোড করা যায়), তবে দ্রুত-এবং-নোংরা প্রবেশ নিয়ন্ত্রণের জন্য বাস্তবায়ন করা সরল।

যদি নিরাপত্তা নিয়ে চিন্তা থাকে (এবং থাকা উচিত), আপনি OAuth, JWT বা API কীস মতো আরো দৃঢ় পদ্ধতিতে মোড় নেবেন। তবুও, বেসিক অথ অংশত পুরানো সিস্টেমগুলির কারণে এবং আংশিকভাবে আপনার নিয়ন্ত্রণে থাকা অভ্যন্তরীণ সিস্টেমের জন্য টিকে আছে।

PHP তে, cURL HTTP অনুরোধ করার জন্য ব্যাপকভাবে ব্যবহৃত হয়, তবে `file_get_contents` বা Guzzle (একটি PHP HTTP ক্লায়েন্ট) এর মতো বিকল্পও বিদ্যমান। `file_get_contents` ব্যবহার করার সময়, উপযুক্ত হেডার সহ একটি কনটেক্সট তৈরি করতে হবে:

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$response = file_get_contents($url, false, $context);

echo $response;
?>
```

আপনার প্রোজেক্টের চাহিদা এবং নিয়ন্ত্রণ এবং ফাংশনালিটির স্তর অনুযায়ী সঠিক টুল বেছে নেওয়ার প্রয়োজন।

## আরো দেখুন

আরও গভীরে যেতে এবং আপনার জ্ঞান বাড়াতে, এগুলি দেখুন:

- [cURL ডকুমেন্টেশন](https://www.php.net/manual/en/book.curl.php)
- [Guzzle ডকুমেন্টেশন](http://docs.guzzlephp.org/en/stable/)
- [PHP `file_get_contents` ফাংশন](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP সহ HTTP অথেন্টিকেশন](https://www.php.net/manual/en/features.http-auth.php)
