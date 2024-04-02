---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:43.454461-06:00
description: "JSON, \u09AC\u09BE JavaScript Object Notation, \u098F\u0995\u099F\u09BF\
  \ \u09B9\u09BE\u09B2\u0995\u09BE \u09A1\u09C7\u099F\u09BE-\u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u099A\u09C7\u099E\u09CD\u099C \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\
  \u099F \u09AF\u09BE \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\
  \u09BE \u098F\u09AC\u0982 \u09B2\u09C7\u0996\u09BE, \u098F\u09AC\u0982 \u09AE\u09C7\
  \u09B6\u09BF\u09A8\u09C7\u09B0 \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982\
  \ \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE \u09B8\u09B9\u099C\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3\
  \u2026"
lastmod: '2024-03-17T18:47:44.154926-06:00'
model: gpt-4-0125-preview
summary: "JSON, \u09AC\u09BE JavaScript Object Notation, \u098F\u0995\u099F\u09BF\
  \ \u09B9\u09BE\u09B2\u0995\u09BE \u09A1\u09C7\u099F\u09BE-\u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u099A\u09C7\u099E\u09CD\u099C \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\
  \u099F \u09AF\u09BE \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\u09B0 \u09AA\u09A1\u09BC\
  \u09BE \u098F\u09AC\u0982 \u09B2\u09C7\u0996\u09BE, \u098F\u09AC\u0982 \u09AE\u09C7\
  \u09B6\u09BF\u09A8\u09C7\u09B0 \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982\
  \ \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09BE \u09B8\u09B9\u099C\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3\
  \u2026"
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?
JSON, বা JavaScript Object Notation, একটি হালকা ডেটা-ইন্টারচেঞ্জ ফর্ম্যাট যা মানুষের পড়া এবং লেখা, এবং মেশিনের পার্স এবং জেনারেট করা সহজ। প্রোগ্রামারগণ সার্ভার এবং ওয়েব অ্যাপ্লিকেশনের মধ্যে ডেটা আদান-প্রদানের জন্য JSON এর সাথে কাজ করেন এর সারল্য এবং ভাষা-স্বাধীনতা কারণে, যা এটিকে আধুনিক ওয়েব ডেভেলপমেন্ট এবং APIs এর একটি কোণপাথরে পরিণত করেছে।

## কিভাবে:
PHP তে JSON এর সাথে কাজ করা অনেক সহজ, ধন্যবাদ `json_encode()` এবং `json_decode()` নির্মিত ফাংশনগুলিকে। নীচে উদাহরণগুলি দেখানো হলো, যেখানে একটি PHP অ্যারেকে JSON স্ট্রিং এ রূপান্তর করা হয়েছে, এবং উল্টোভাবে:

### একটি PHP অ্যারেকে JSON স্ট্রিং এ এনকোডিং
```php
// একটি অ্যাসোসিয়েটিভ অ্যারে সংজ্ঞায়িত করুন
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// PHP অ্যারেকে একটি JSON স্ট্রিং এ রূপান্তর করুন
$jsonString = json_encode($data);

// JSON স্ট্রিং আউটপুট করুন
echo $jsonString;
```
**নমুনা আউটপুট:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### একটি JSON স্ট্রিং কে PHP অ্যারে তে ডিকোডিং
```php
// JSON স্ট্রিং
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// JSON স্ট্রিং কে PHP অ্যারে তে রূপান্তর করুন
$data = json_decode($jsonString, true);

// PHP অ্যারে আউটপুট করুন
print_r($data);
```
**নমুনা আউটপুট:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### একটি থার্ড-পার্টি লাইব্রেরি সাথে কাজ: GuzzleHttp
জটিল JSON এবং ওয়েব অনুরোধ পরিচালনার জন্য, একটি জনপ্রিয় PHP লাইব্রেরি হল GuzzleHttp। এটি HTTP অনুরোধগুলি সহজতর করে এবং JSON ডেটা সহ সহজে কাজ করে।

**কম্পোজার এর মাধ্যমে ইনস্টলেশন:**
```
composer require guzzlehttp/guzzle
```

**উদাহরণ অনুরোধ:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// একটি API কে অনুরোধ পাঠান যা JSON ফেরত দেয়
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// JSON প্রতিক্রিয়াকে PHP অ্যারে তে ডিকোড করুন
$data = json_decode($response->getBody(), true);

// ডেটা আউটপুট করুন
print_r($data);
```

**ধারণা করা হচ্ছে API অনুরূপ JSON ডেটা ফেরত পাঠায়:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
এটি PHP ব্যবহার করে JSON ম্যানিপুলেশনের সাথে কাজ করার সহজতা দেখায়, নেটিভ ফাংশনগুলির সাথে এবং আরও জটিল কাজের জন্য GuzzleHttp এর মতো দৃঢ় লাইব্রেরিগুলির সাথে।
