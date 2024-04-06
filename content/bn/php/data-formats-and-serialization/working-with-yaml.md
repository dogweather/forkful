---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:32.840157-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PHP \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8\u09C7\u09B0 \u0987\u09A4\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7, YAML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u0995\u09B0\
  \u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u0985\u0982\u09B6\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\
  \u09B0\u09C7 \u09A8\u09BE\u0964 PHP\u09A4\u09C7 YAML \u09A8\u09BF\u09AF\u09BC\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\
  \u09C7 \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09B9\u09B2\u2026"
lastmod: '2024-03-17T18:47:44.153896-06:00'
model: gpt-4-0125-preview
summary: "PHP \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8\u09C7\u09B0 \u0987\u09A4\u09BE\
  \u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u09A4\u09C7, YAML \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u0995\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF\u09B0 \u0985\u0982\u09B6 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u09B8\
  \u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7 \u09A8\u09BE\u0964 PHP\u09A4\u09C7\
  \ YAML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0\
  \ \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09B9\u09B2 Symfony YAML \u0995\u09AE\u09CD\u09AA\u09CB\u09A8\u09C7\
  \u09A8\u09CD\u099F \u09AC\u09BE `yaml` PECL \u098F\u0995\u09CD\u09B8\u099F\u09C7\
  \u09A8\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\
  \u0964\n\n\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, Composer \u098F\u09B0 \u09AE\u09BE\
  \u09A7\u09CD\u09AF\u09AE\u09C7 Symfony YAML \u0995\u09AE\u09CD\u09AA\u09CB\u09A8\
  \u09C7\u09A8\u09CD\u099F \u0987\u09A8\u09CD\u09B8\u099F\u09B2 \u0995\u09B0\u09C1\
  \u09A8."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
PHP বর্তমানের ইতারেশনগুলিতে, YAML পার্সিং করা স্ট্যান্ডার্ড লাইব্রেরির অংশ হিসেবে সমর্থন করে না। PHPতে YAML নিয়ে কাজ করার সবচেয়ে সরল উপায় হল Symfony YAML কম্পোনেন্ট বা `yaml` PECL এক্সটেনশন ব্যবহার করা।

### Symfony YAML কম্পোনেন্ট ব্যবহার করে
প্রথমে, Composer এর মাধ্যমে Symfony YAML কম্পোনেন্ট ইন্সটল করুন:

```bash
composer require symfony/yaml
```

তারপর, YAML কনটেন্ট পার্স এবং ডাম্প করা যায় এইভাবে:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAML পার্সিং
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// অ্যারে থেকে YAML তৈরি করা
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

পার্সিং করার সময় নমুনা আউটপুট:

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

ডাম্প করার সময় নমুনা আউটপুট:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### `yaml` PECL এক্সটেনশন ব্যবহার করে
আপনি যদি পছন্দ করেন, বা আপনার প্রকল্পের প্রয়োজন যদি অনুমোদন দেয়, তাহলে PECL এক্সটেনশন YAML নিয়ে কাজ করার আরেকটি কার্যকরী উপায় হতে পারে। প্রথমে, এক্সটেনশনটি ইনস্টল করা আছে কিনা নিশ্চিত করুন:

```bash
pecl install yaml
```

তারপর, আপনার `php.ini` কনফিগারেশনে এটি সক্রিয় করুন:

```ini
extension=yaml.so
```

YAML পার্স এবং এমিট করা এইভাবে করা যায়:

```php
<?php

// YAML পার্সিং
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// অ্যারে থেকে YAML তৈরি করা
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

আউটপুট Symfony কম্পোনেন্টের মত হবে, YAML এর ভূমিকা মানুষের পড়ার উপযোগী ফর্ম্যাট এবং PHP অ্যারে কাঠামোর মাঝে একটা সেতুর মত কাজ করে, যা কনফিগারেশন এবং ডাটা পরিচালনা সহজ করে তোলে।
