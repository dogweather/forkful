---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:49.293710-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B8\u09C1\u09A8 \u09A6\
  \u09C1\u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09AF\u09CB\u0997\
  \u09AB\u09B2 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0982\u09B6\u09A8 \u09B2\
  \u09BF\u0996\u09BF\u0964 \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3, \u0995\u09BF\u09A8\
  \u09CD\u09A4\u09C1 \u098F\u099F\u09BF \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\
  \u09CD\u09A4\u09C1\u0995\u09C7 \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\
  \ \u0995\u09B0\u09C7\u0964."
lastmod: '2024-03-17T18:47:44.282650-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B8\u09C1\u09A8 \u09A6\u09C1\u099F\u09BF \u09B8\u0982\u0996\u09CD\
  \u09AF\u09BE\u09B0 \u09AF\u09CB\u0997\u09AB\u09B2 \u09B9\u09BF\u09B8\u09C7\u09AC\
  \u09C7 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09B2\u09BF\u0996\u09BF\u0964 \u09B8\u09BE\u09A7\
  \u09BE\u09B0\u09A3, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u098F\u099F\u09BF \u09AC\
  \u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7 \u09AA\u09CD\u09B0\
  \u09A6\u09B0\u09CD\u09B6\u09A8 \u0995\u09B0\u09C7\u0964."
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কিভাবে:
আসুন দুটি সংখ্যার যোগফল হিসেবে করার জন্য একটি ফাংশন লিখি। সাধারণ, কিন্তু এটি বিষয়বস্তুকে প্রদর্শন করে।

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# ফাংশনটি 5 এবং 10 দিয়ে কল করুন
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "যোগফল হল $sum"
```

নমুনা আউটপুট:

```
যোগফল হল 15
```

## গভীরে ডুব:
PowerShell-এ ফাংশন, অধিকাংশ ভাষায় যেমন, পুরানো খবর। আমরা Fortran-এর দিন থেকেই কোডকে বিভাগায়িত করে আসছি। এটি 'চাকা পুনরাবিষ্কার না করা' সম্পর্কে। বিকল্প? অবশ্যই, স্ক্রিপ্ট বা সিমলেটস। কিন্তু তাদের মধ্যে স্ক্রিপ্টগুলির মধ্যে ফাংশনগুলির পরিষ্কারতা এবং প্রসঙ্গ-সচেতনতা অভাব রয়েছে।

বাস্তবায়ন? ফাংশনগুলি আমাদের উদাহরণের মতো সাধারণ হতে পারে বা স্কোপ, পাইপলাইন ইনপুট এবং আরও অনেক কিছুর সাথে জটিল হতে পারে। ‘এডভান্সড ফাংশনস’ নিন। এগুলো সিমলেটগুলির মতো আচরণ করে, যার প্যারামিটারগুলিতে অ্যাট্রিবিউট থাকে, যেমন `[Parameter(Mandatory=$true)]।` এটি PowerShell-এর নমনীয়তার একটি স্বাদ।

## আরও দেখুন
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
