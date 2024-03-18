---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:48.796091-06:00
description: "PowerShell \u098F \u09AF\u09C7\u09AD\u09BE\u09AC\u09C7 \u09B0\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A4\u09BE \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09B0\u09BF\u09B8\u09C0\u09AE\u09BE\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u0985\u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\
  \u09B6\u09BF\u09A4 \u09B8\u09BE\u0982\u0996\u09CD\u09AF\u09BF\u0995 \u09AE\u09BE\
  \u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7\
  \ \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09A8\u09BF\u09AE\u09CD\
  \u09A8\u09B2\u09BF\u0996\u09BF\u09A4\u2026"
lastmod: '2024-03-17T18:47:44.267387-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F \u09AF\u09C7\u09AD\u09BE\u09AC\u09C7 \u09B0\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A4\u09C8\
  \u09B0\u09BF \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A4\u09BE \u09A8\u09BF\u09B0\
  \u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09B0\u09BF\u09B8\u09C0\u09AE\u09BE\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u0985\u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\
  \u09B6\u09BF\u09A4 \u09B8\u09BE\u0982\u0996\u09CD\u09AF\u09BF\u0995 \u09AE\u09BE\
  \u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7\
  \ \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09BF\u09A4\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u0997\u09A3 \u09A8\u09BF\u09AE\u09CD\
  \u09A8\u09B2\u09BF\u0996\u09BF\u09A4\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
PowerShell এ যেভাবে র্যান্ডম সংখ্যা তৈরি করা হয় তা নির্দিষ্ট পরিসীমার মধ্যে অপ্রত্যাশিত সাংখ্যিক মান তৈরি করা নিয়ে সম্পর্কিত। প্রোগ্রামারগণ নিম্নলিখিত কারণগুলির জন্য এই ক্ষমতাটি ব্যবহার করেন: পরীক্ষামূলক উদ্দেশ্য, সিমুলেশন, এবং নিরাপত্তা, যেখানে অপ্রত্যাশিততা বা বাস্তব জগতের যাদুগরি অনুকরণ করা অপরিহার্য।

## কিভাবে:
PowerShell `Get-Random` cmdlet ব্যবহার করে র্যান্ডম সংখ্যা উৎপন্ন করার জন্য একটি সরল পদ্ধতি অফার করে। এই cmdlet একটি নির্দিষ্ট পরিসীমা বা ডিফল্ট পরিসীমার মধ্যে র্যান্ডম সংখ্যা উৎপন্ন করতে পারে।

```PowerShell
# 0 থেকে Int32.MaxValue পর্যন্ত একটি র্যান্ডম সংখ্যা উৎপন্ন করুন
$randomNumber = Get-Random
Write-Output $randomNumber
```

একটি পরিসীমা নির্দিষ্ট করতে, `-Minimum` এবং `-Maximum` প্যারামিটার ব্যবহার করুন:

```PowerShell
# 1 থেকে 100 পর্যন্ত একটি র্যান্ডম সংখ্যা উৎপন্ন করুন
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

আরও বেশি নিয়ন্ত্রণের জন্য, আপনি `System.Random` ক্লাসের একটি অবজেক্ট তৈরি করতে পারেন:

```PowerShell
# একটি সিরিজের জন্য System.Random ব্যবহার করা
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

আপনি যদি একটি অ্যারে বা সংগ্রহ থেকে র্যান্ডম নির্বাচন প্রয়োজন হয়, `Get-Random` সরাসরি একটি আইটেম বাছাই করতে পারে:

```PowerShell
# একটি অ্যারে থেকে র্যান্ডম নির্বাচন
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## গভীর ডুব
PowerShell এর `Get-Random` cmdlet .NET ক্লাস `System.Random` ব্যবহার করে pseudorandom সংখ্যা উৎপন্ন করে। এগুলি "প্সিউডো" কারণ এগুলি এমন সিরিজ তৈরি করতে এলগোরিদম ব্যবহার করে যা শুধু র্যান্ডম হিসেবে মনে হয়। অধিকাংশ অ্যাপ্লিকেশনের জন্য, এই ধরনের র্যান্ডমনেস যথেষ্ট। তবে, ক্রিপ্টোগ্রাফিক নিরাপত্তা প্রয়োজনীয় ক্ষেত্রে, `System.Random` তার প্রেডিক্টেবল প্রকৃতির কারণে উপযুক্ত নয়।

PowerShell এবং .NET `System.Security.Cryptography.RNGCryptoServiceProvider` ক্রিপ্টোগ্রাফিক র্যান্ডমনেসের জন্য প্রস্তাবনা করে যা এনক্রিপশন কী অথবা অন্যান্য নিরাপত্তা-সংবেদনশীল অপারেশন তৈরির জন্য আরও উপযুক্ত:

```PowerShell
# ক্রিপ্টোগ্রাফিকভাবে নিরাপদ র্যান্ডম সংখ্যা
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

যদিও `Get-Random` এবং `System.Random` স্ক্রিপ্টিং এবং অ্যাপ্লিকেশন লজিকে র্যান্ডমনেসের বিস্তৃত সেটের চাহিদা মেটায়, বিশেষ করে নিরাপত্তা-কেন্দ্রিক অ্যাপ্লিকেশনগুলিতে যেখানে প্রেডিক্ট্যাবিলিটি একটি ভালনারাবিলিটি তৈরি করতে পারে সেখানে সঠিক টুল নির্বাচন করা অপরিহার্য।
