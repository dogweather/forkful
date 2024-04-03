---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:23.315903-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP \u09B8\u09BF\u098F\u09B8\u09AD\
  \u09BF \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u09B8\u09AE\u09CD\u09AD\
  \u09BE\u09B2\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F\
  -\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8\
  \ \u0995\u09B0\u09C7, \u09AF\u09BE \u098F\u0987 \u09AB\u09BE\u0987\u09B2\u0997\u09C1\
  \u09B2\u09BF\u09A4\u09C7 \u09AA\u09A1\u09BC\u09A4\u09C7 \u098F\u09AC\u0982 \u09B2\
  \u09BF\u0996\u09A4\u09C7 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\
  \u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE\
  \u0987 \u09B8\u09B0\u09B2 \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.155936-06:00'
model: gpt-4-0125-preview
summary: "PHP \u09B8\u09BF\u098F\u09B8\u09AD\u09BF \u09AB\u09BE\u0987\u09B2\u0997\u09C1\
  \u09B2\u09BF \u09B8\u09AE\u09CD\u09AD\u09BE\u09B2\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE \u098F\u0987\
  \ \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AA\u09A1\u09BC\
  \u09A4\u09C7 \u098F\u09AC\u0982 \u09B2\u09BF\u0996\u09A4\u09C7 \u09A4\u09C3\u09A4\
  \u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\
  \ \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u09B8\u09B0\u09B2 \u0995\u09B0\u09C7 \u09A4\
  \u09CB\u09B2\u09C7\u0964 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09A4\u09C7 \u09A8\
  \u09BF\u099A\u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u0997\u09C1\u09B2\u09BF\
  \ \u09A6\u09C7\u0996\u09C1\u09A8."
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কিভাবে:
PHP সিএসভি ফাইলগুলি সম্ভালার জন্য বিল্ট-ইন ফাংশন প্রদান করে, যা এই ফাইলগুলিতে পড়তে এবং লিখতে তৃতীয়-পক্ষের লাইব্রেরির প্রয়োজন ছাড়াই সরল করে তোলে। শুরু করতে নিচের উদাহরণগুলি দেখুন:

### একটি CSV ফাইল পড়া
`fopen()` এর সঙ্গে `fgetcsv()` ব্যবহার করে একটি CSV ফাইল ওপেন করে এর কন্টেন্ট পড়তে পারেন:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "লাইনে ফিল্ডের সংখ্যা: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

এই স্ক্রিপ্ট প্রতিটি লাইনের ফিল্ডের সংখ্যা এবং প্রতিটি ফিল্ডের কন্টেন্ট প্রিন্ট করে।

### একটি CSV ফাইলে লেখা
CSV ফাইলে লেখার জন্য, `fopen()` কে লেখার মোড (`w`) এ এবং `fputcsv()` ব্যবহার করুন:

```php
<?php
$list = [
    ['ID', 'Name', 'Email'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

এই স্ক্রিপ্টটি `users.csv` নামে একটি ফাইল তৈরি করে এবং এতে হেডার এবং দুটি ডেটা সারি লেখে।

### লাইব্রেরি ব্যবহার: League\Csv
আরও উন্নত CSV হ্যান্ডলিংয়ের জন্য, `League\Csv` লাইব্রেরি একটি শক্তিশালী ফিচার সেট অফার করে। Composer (`composer require league/csv`) এর মাধ্যমে ইনস্টল করার পর, আপনি CSV ডেটা আরও নমনীয়ভাবে পড়তে এবং লিখতে তা ব্যবহার করতে পারেন।

#### League\Csv দিয়ে পড়া
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // যদি আপনি প্রথম সারিকে হেডার হিসেবে ব্যবহার করতে চান

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

এই স্ক্রিপ্টটি `data.csv` পড়ে, প্রথম সারিকে কলাম হেডার হিসেবে বিবেচনা করে এবং প্রতিটি সারিকে একটি এসোসিয়েটিভ অ্যারে হিসেবে প্রিন্ট করে।

#### League\Csv দিয়ে লেখা
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Name', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "users_new.csv এ সফলভাবে লেখা হয়েছে।";
?>
```

এটি `users_new.csv` তৈরি করে এবং এতে একটি হেডার সারি অনুসরণে দুটি ডেটা সারি লেখে।
