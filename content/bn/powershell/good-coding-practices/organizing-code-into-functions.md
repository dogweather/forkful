---
title:                "কোডকে ফাংশনগুলিতে আয়োজন করা"
date:                  2024-03-17T17:51:49.293710-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
কোডকে ফাংশনগুলিতে আয়োজন করার অর্থ হ'ল নির্দিষ্ট কাজ সম্পাদনের জন্য কোডের চাঙ্কগুলি মোড়ানো এবং তাদের একটি নাম দেওয়া। এটি কোডকে পুন:ব্যবহারযোগ্য, পাঠযোগ্য এবং বজায় রাখার জন্য করা হয়। একই কোড পুনরায় লিখতে না গিয়ে, একটি ফাংশন কল করুন। সমস্যার সমাধান বা আপগ্রেড করতে চান? বিস্তর স্ক্রিপ্ট ঘাঁটাঘাঁটি না করে ফাংশনটি সামান্য পরিবর্তন করুন।

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
