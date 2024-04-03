---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:07:50.611777-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: ."
lastmod: '2024-03-17T18:47:44.132877-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u09A1\u09BF\u09AC\u09BE\u0997 \u0986\u0989\u099F\u09AA\u09C1\u099F \u09AA\
  \u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09BE"
weight: 33
---

## কিভাবে:
```PHP
<?php
// মৌলিক আউটপুট
$variable = 'ডিবাগিং রক্স!';
echo $variable;

// অ্যারের জন্য print_r ব্যবহার করা
$myArray = ['apple', 'orange', 'banana'];
echo '<pre>'; // এটি পড়তে সুবিধাজনক করে
print_r($myArray);
echo '</pre>';

// বিস্তারিত জন্য var_dump
$anotherArray = ['key' => 'value', 'anotherKey' => 123];
var_dump($anotherArray);

// এরর লগে যায়
error_log('এটি লগে যায়, আরও গোপনীয় ডিবাগিংয়ের জন্য।');
?>
```
নমুনা আউটপুট:
```
ডিবাগিং রক্স!
অ্যারে
(
    [0] => apple
    [1] => orange
    [2] => banana
)
array(2) {
  ["key"]=>
  string(5) "value"
  ["anotherKey"]=>
  int(123)
}
```

## গভীরে নিবিদ:
ডিবাগ আউটপুট খুব একটা পরিবর্তিত হয়নি: পুরানো দিন থেকেই এটি আছে যখন প্রাচীন প্রোগ্রামাররা printf() দিয়ে ডিবাগ করত। PHP এ `echo`, `print`, `print_r()`, এবং `var_dump()` দিয়ে এটি গ্রহণ করেছে। এটি হয়তো অভিজাত নয়, কিন্তু কাজ করে। আধুনিক PHP ডেভেলপাররা আরও Xdebug হাতে নিয়েছে, যা কোডের মাধ্যমে ধাপে ধাপে একটি ফ্যান্সি আউটপুট দেখাতে পারে। লগের জন্য, আপনি `error_log()` পেয়েছেন, যা ব্যবহারকারীদের কাছে বার্তাগুলি প্রকাশ না করে সার্ভার লগে বার্তা পাঠায়। প্রতিটি টুলের তার নিজস্ব স্থান রয়েছে: `echo` এবং `print` দ্রুত এবং নোংরা; `print_r()` মানুষের জন্য বান্ধব অ্যারে ইনসাইটের জন্য; `var_dump()` আপনাকে টাইপ এবং লেন্থের গভীর তথ্য দেয়; `error_log()` জীবন্ত সাইটে গোয়েন্দার মোডে থাকাকালীন বিষয়গুলি আড়াল করে রাখে।

## আরও দেখুন:
- `echo` নিয়ে PHP ম্যানুয়াল: https://www.php.net/manual/en/function.echo.php
- `print_r()` সম্পর্কে আরও: https://www.php.net/manual/en/function.print-r.php
- `var_dump()`-এর গভীর তথ্য: https://www.php.net/manual/en/function.var-dump.php
- `error_log()` দিয়ে লগিংয়ে ডুব দেওয়া: https://www.php.net/manual/en/function.error-log.php
- Xdebug, ডিবাগারের সেরা বন্ধু: https://xdebug.org/docs/display
