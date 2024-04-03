---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:57.659144-06:00
description: "TOML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B0\u09C2\
  \u09AA \u09B9\u09B2 Tom's Obvious, Minimal Language, \u098F\u0995\u099F\u09BF \u09A1\
  \u09C7\u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\
  \u09C7\u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u098F\
  \u09B0 \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\u09BE\u09A8\u09CD\
  \u099F\u09BF\u0995\u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\
  \u09BC\u09A4\u09C7 \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.302823-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u09AF\u09BE\u09B0 \u09AA\u09C2\u09B0\u09CD\u09A3 \u09B0\u09C2\u09AA\
  \ \u09B9\u09B2 Tom's Obvious, Minimal Language, \u098F\u0995\u099F\u09BF \u09A1\u09C7\
  \u099F\u09BE \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\
  \u09B6\u09A8 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u098F\u09B0\
  \ \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\u09BE\u09A8\u09CD\u099F\
  \u09BF\u0995\u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\
  \u09A4\u09C7 \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  , \u0995\u09BE\u09B0\u09A3 \u098F\u099F\u09BF \u09AE\u09BE\u09A8\u09C1\u09B7\u09C7\
  \u09B0 \u09AA\u09A1\u09BC\u09BE\u09B0 \u0989\u09AA\u09AF\u09CB\u0997\u09C0 \u098F\
  \u09AC\u0982 \u09AE\u09C7\u09B6\u09BF\u09A8-\u09AC\u09BE\u09A8\u09CD\u09A7\u09AC\
  \ \u09B9\u0993\u09AF\u09BC\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u098F\u0995\
  \u099F\u09BF \u09AD\u09BE\u09B0\u09B8\u09BE\u09AE\u09CD\u09AF \u09A4\u09C8\u09B0\
  \u09BF \u0995\u09B0\u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি ও কেন?

TOML, যার পূর্ণ রূপ হল Tom's Obvious, Minimal Language, একটি ডেটা সিরিয়ালাইজেশন ফরম্যাট যা এর স্পষ্ট সিমান্টিকসের কারণে পড়তে সহজ। প্রোগ্রামাররা এটি কনফিগারেশন ফাইলের জন্য ব্যবহার করে, কারণ এটি মানুষের পড়ার উপযোগী এবং মেশিন-বান্ধব হওয়ার মধ্যে একটি ভারসাম্য তৈরি করে।

## কিভাবে:

PowerShell-এ, TOML পার্স করার জন্য কোনো নেটিভ cmdlet নেই। আপনি যদি PowerShell এর সাথে কাজ করতে চান, তবে আপনি সাধারণত একটি মডিউল ব্যবহার করবেন অথবা `toml-to-json` এর মতো একটি টুল দিয়ে TOML কে JSON এ রূপান্তরিত করবেন। এখানে একটি কল্পিত মডিউল `PowerShellTOML` ব্যবহার করে আপনি কিভাবে করতে পারেন তা দেখানো হল:

```PowerShell
# প্রথমে, মডিউল ইনস্টল করুন (কল্পিত, ডেমোনস্ট্রেশনের জন্য)
Install-Module PowerShellTOML

# একটি TOML ফাইল ইম্পোর্ট করুন
$config = Import-TomlConfig -Path './config.toml'

# একটি মান অ্যাক্সেস করা
Write-Output $config.database.server

# 'config.toml' এর মধ্যে স্যাম্পল TOML কন্টেন্ট:
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# স্যাম্পল আউটপুট:
# 192.168.1.1
```

## গভীর ডুব

TOML তৈরি করা হয়েছিল Tom Preston-Werner দ্বারা, যিনি GitHub-এর সহ-প্রতিষ্ঠাতা, কনফিগারেশন ফাইলের জন্য XML এবং YAML এর তুলনায় একটি সরল বিকল্প হিসেবে। এর প্রথম সংস্করণ ২০১৩ সালে প্রকাশিত হয়েছিল। TOML JSON এর তুলনায় তুলনীয় কিন্তু মানুষের পক্ষে অধিক বন্ধুত্বপূর্ণ ডিজাইন করা হয়েছে, যা এটিকে মানুষ দ্বারা পরিচালিত কনফিগারেশনের জন্য একটি ভালো পছন্দ করে তোলে। বিকল্পগুলি অন্তর্ভুক্ত করে YAML, JSON, এবং XML।

বাস্তবায়নের ক্ষেত্রে, TOML এর জন্য একটি PowerShell মডিউল সাধারণত C# এর মতো আরও কর্মক্ষমতা-ভিত্তিক ভাষায় লেখা একটি TOML লাইব্রেরির চারপাশে একটি র‍্যাপার হিসেবে কাজ করে। PowerShell-এ TOML-এর জন্য বিল্ট-ইন সাপোর্ট না থাকায়, TOML ফরম্যাটের সাথে সুবিধাজনকভাবে ইন্টারফেস করার জন্য এমন একটি মডিউল প্রয়োজন।

## আরও দেখুন

- TOML স্ট্যান্ডার্ড: https://toml.io/en/
- `toml` PowerShell মডিউলের জন্য GitHub রিপোজিটরি (পঠনের সময় যদি অস্তিত্ব থাকে): https://github.com/powershell/PowerShellTOML
- TOML সম্পর্কে পরিচিতি: https://github.com/toml-lang/toml
- ডেটা সিরিয়ালাইজেশন ফরম্যাটের তুলনা: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
