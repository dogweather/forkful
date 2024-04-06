---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:55.563659-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 PowerShell \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\
  \u09A8\u09CD\u099F\u09B8\u0997\u09C1\u09B2\u09BF `$args` \u0985\u09CD\u09AF\u09BE\
  \u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u09AA\u09CD\u09AF\u09BE\u09B0\u09BE\u09AE\
  \u09BF\u099F\u09BE\u09B0\u09A6\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09AA\u09A1\u09BC\u09C7\u0964 `$args` \u098F\u0995\u09AC\u09BE\u09B0\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09A6\u09CD\u09B0\u09C1\
  \u09A4;\u2026"
lastmod: '2024-04-05T22:38:51.660888-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 PowerShell \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\
  \u09A8\u09CD\u099F\u09B8\u0997\u09C1\u09B2\u09BF `$args` \u0985\u09CD\u09AF\u09BE\
  \u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u09AA\u09CD\u09AF\u09BE\u09B0\u09BE\u09AE\
  \u09BF\u099F\u09BE\u09B0\u09A6\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u09AA\u09A1\u09BC\u09C7\u0964 `$args` \u098F\u0995\u09AC\u09BE\u09B0\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u09A6\u09CD\u09B0\u09C1\
  \u09A4; \u09AA\u09CD\u09AF\u09BE\u09B0\u09BE\u09AE\u09BF\u099F\u09BE\u09B0\u0997\
  \u09C1\u09B2\u09BF \u09A6\u09C3\u09A2\u09BC \u099F\u09C1\u09B2\u09B8\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AD\u09BE\u09B2\u09CB\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কিভাবে
PowerShell কমান্ড লাইন আর্গুমেন্টসগুলি `$args` অ্যারে অথবা প্যারামিটারদের মাধ্যমে পড়ে। `$args` একবারের জন্য স্ক্রিপ্টগুলির জন্য দ্রুত; প্যারামিটারগুলি দৃঢ় টুলসের জন্য ভালো।

### `$args` ব্যবহার করা
```PowerShell
# myscript.ps1
Write-Host "আপনি নিম্নলিখিত আর্গুমেন্টস প্রবেশ করেছেন:"
$args
```
`.\myscript.ps1 Hello PowerShell` দিয়ে চালিয়ে, আউটপুট হবে:
```
আপনি নিম্নলিখিত আর্গুমেন্টস প্রবেশ করেছেন:
Hello PowerShell
```

### প্যারামিটারস ব্যবহার করা
```PowerShell
# myscriptparam.ps1
param (
    [string]$Name,
    [int]$Age
)
Write-Host "হ্যালো, $Name! তুমি $Age বছর বয়সী।"
```
`.\myscriptparam.ps1 -Name Sarah -Age 32` দিয়ে চালিয়ে, আউটপুট হবে:
```
হ্যালো, Sarah! তুমি 32 বছর বয়সী।
```

## গভীর ডুব
PowerShell এর কমান্ড লাইন আর্গুমেন্টসের আধুনিক পদ্ধতি তার পূর্বসূরিদের যেমন cmd এবং Bash এর থেকে একটি উত্তরাধিকার। তবে, এটি নমনীয়তা এবং নির্ভুলতা বৃদ্ধি করে।

### ঐতিহাসিক প্রেক্ষাপট
বছর আগে, ব্যাচ ফাইল এবং শেল স্ক্রিপ্টগুলি নাম্বারযুক্ত চলক (যেমন `%1`, `%2`) দ্বারা আর্গুমেন্টস অ্যাকসেস করত। PowerShell এ এগুলির উন্নতি করে `$args` এবং নামযুক্ত প্যারামিটারগুলি দ্বারা আরও পরিষ্কার এবং নিয়ন্ত্রণাধীন করেছে।

### বিকল্পগুলি
`Read-Host` দ্বারা কাঁচা ইনপুট পার্স করা বা পাইপ করা ইনপুট গ্রহণ করা যেমন বিকল্প রয়েছে। তবে, `$args` এবং প্যারামিটারগুলি স্বয়ংক্রিয়কৃত কাজ এবং স্ক্রিপ্টগুলির জন্য আরও সীমাবদ্ধতামুক্ত।

### বাস্তবায়নের বিবরণ
`$args` হল একটি সাধারণ অ্যারে, যেকোনো ইনপুটের জন্য ভালো। প্যারামিটারগুলি, তাদের আকর্ষণ এবং টাইপ দ্বারা, ইনপুট যাচাই করতে পারে এবং এমনকি ব্যবহারকারীকে প্রম্পট করতে পারে, স্ক্রিপ্টগুলিকে স্ব-ডকুমেন্টিং এবং ত্রুটির প্রবণতা কম করে তোলে।

## আরও দেখুন
- [প্যারামিটারস সম্পর্কে](https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/cmdlet-parameter-sets?view=powershell-7)
- [PowerShell এ অটোমেটিক ভেরিয়েবলস](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7&viewFallbackFrom=powershell-6)
