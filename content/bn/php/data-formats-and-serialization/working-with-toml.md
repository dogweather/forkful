---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:52.113235-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 \u098F\u0995\u099F\u09BF TOML\
  \ \u09AA\u09BE\u09B0\u09B8\u09BE\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0986\u099B\u09C7, \u09AF\u09C7\
  \u09AE\u09A8 `yosymfony/toml`\u0964 \u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF\
  \ TOML \u09AB\u09BE\u0987\u09B2 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BF\
  ."
lastmod: '2024-03-17T18:47:44.156947-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\u09BF\u09B6\u09CD\u099A\u09BF\
  \u09A4 \u0995\u09B0\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\
  \u09C7 \u098F\u0995\u099F\u09BF TOML \u09AA\u09BE\u09B0\u09B8\u09BE\u09B0 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2\
  \ \u0986\u099B\u09C7, \u09AF\u09C7\u09AE\u09A8 `yosymfony/toml`\u0964 \u099A\u09B2\
  \u09C1\u09A8 \u098F\u0995\u099F\u09BF TOML \u09AB\u09BE\u0987\u09B2 \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09BF."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
প্রথমে, নিশ্চিত করুন আপনার কাছে একটি TOML পারসার লাইব্রেরি ইনস্টল আছে, যেমন `yosymfony/toml`। চলুন একটি TOML ফাইল পার্স করি:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

নমুনা আউটপুট:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```

## গভীরে ডুব:
TOML আবির্ভাব ঘটে ২০১৩ সালে, GitHub সহ-প্রতিষ্ঠাতা টম প্রেস্টন-ওয়ার্নার দ্বারা তৈরি, XML এবং JSON এর তুলনায় কনফিগ ফাইলের জন্য একটি ব্যবহারকারী বান্ধব বিকল্প হিসেবে। মেশিনের জন্য JSON সহজ হলেও, TOML এর কাঠামো মানব চোখের জন্য সহজ, YAML এর জটিলতা ব্যতিরেকে।

TOML এর বিকল্পগুলি হল JSON, YAML, এবং XML। প্রত্যেকেরই তাদের নিজের শক্তি ও প্রয়োগ দৃশ্যাবলী রয়েছে। JSON ব্যাপকভাবে ব্যবহৃত এবং ভাষা-নিরপেক্ষ; YAML আরও পঠনীয় এবং মন্তব্য সমর্থন করে, অন্যদিকে XML ব্যাপক এবং ব্যাপকভাবে সমর্থিত।

PHP তে TOML বাস্তবায়ন করার সময়, আপনি PHP অ্যারে বা অবজেক্টে এর কন্টেন্ট পার্স করার লাইব্রেরিগুলি দেখছেন। `yosymfony/toml` একটি PHP পারসার যা TOML স্পেসিফিকেশনের v0.4.0 কে মেনে চলে। সর্বশেষের সাথে থাকতে, সর্বশেষ TOML সংস্করণ (v1.0.0 আমার শেষ আপডেট পর্যন্ত) সমর্থন করে এমন নতুন পারসার বা আপডেটগুলির জন্য সবসময় খোঁজ করুন।

## দেখুন আরো
- TOML স্পেসিফিকেশন: <https://toml.io/>
- TOML পারসার ফর PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- ডাটা ফরম্যাট তুলনা (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP প্যাকেজ ম্যানেজার (কম্পোজার): <https://getcomposer.org/>
