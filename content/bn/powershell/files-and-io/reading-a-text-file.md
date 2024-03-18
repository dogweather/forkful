---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:19.542109-06:00
description: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\
  \u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B0\
  \ \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7 \u098F\
  \u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\u09B0\u09CD\u09AE\u09C7 \u099F\u09C7\
  \u09A8\u09C7 \u0986\u09A8\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09B8\u09BE\u09A5\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8\u09BF\u0982, \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u09B2\u0997\u09BF\u0982 -\u2026"
lastmod: '2024-03-17T18:47:44.295975-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09AA\u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B0 \u09AC\
  \u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7 \u098F\u09AE\u09A8\
  \ \u098F\u0995\u099F\u09BF \u09AB\u09B0\u09CD\u09AE\u09C7 \u099F\u09C7\u09A8\u09C7\
  \ \u0986\u09A8\u09BE \u09AF\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8\u09BF\u0982, \u0995\u09A8\u09AB\u09BF\u0997\
  \u09BE\u09B0\u09C7\u09B6\u09A8, \u09B2\u0997\u09BF\u0982 -\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি টেক্সট ফাইল পড়া মানে এর বিষয়বস্তুকে এমন একটি ফর্মে টেনে আনা যা আপনার প্রোগ্রাম সাথে কাজ করতে পারে। প্রোগ্রামাররা ডেটা প্রসেসিং, কনফিগারেশন, লগিং - যখনই একটি মানব-পাঠ্য ফাইল জড়িত থাকে তখন তারা এটি করে।

## কিভাবে:
আসুন মৌলিক নিয়মাবলী দিয়ে শুরু করি! এখানে দেখানো হলো PowerShell এ একটি টেক্সট ফাইল থেকে কিভাবে পড়বেন:

```PowerShell
# একটি ফাইলের বিষয়বস্তু পেতে
$content = Get-Content -Path "C:\path\to\your\file.txt"
# কনসোলে বিষয়বস্তুটি প্রদর্শন
Write-Output $content
```

আপনার ফাইলে যদি কয়েক লাইনের টেক্সট থাকে তাহলে নমুনা আউটপুট এমন দেখাবে:
```
হ্যালো, PowerShell!
ফাইলের শেষ।
```

এখন, লাইন অনুযায়ী পড়তে চান?

```PowerShell
# ফাইলটি লাইন অনুযায়ী পড়ুন
$lines = Get-Content -Path "C:\path\to\your\file.txt" -ReadCount 0
foreach ($line in $lines) {
    Write-Output $line
}
```

উপরের একই নমুনা আউটপুট, কিন্তু এক লাইন করে প্রক্রিয়াজাত।

## গভীর ডুব
PowerShell আসার অনেক আগে, UNIX সিস্টেমে `cat` বা DOS এ `type` এর মতো কমান্ড-লাইন টুলস ফাইল পড়ার জন্য প্রথম পছন্দ ছিল। আজকে, PowerShell এর Get-Content হলো এর জন্য ধারালো টুল, যেমন লাইন অনুযায়ী পড়ার মতো অতিরিক্ত সুবিধাসমূহ রয়েছে, যা বিশাল ফাইলগুলির সাথে মেমোরি বোঝায় ভরাট এড়াতে সাহায্য করে।

`Get-Content` ছাড়াও, আরও নিয়ন্ত্রণের জন্য আমাদের হাতে `.NET` ক্লাসগুলি রয়েছে - `System.IO.StreamReader` এ প্রবেশ করুন:

```PowerShell
$stream = [System.IO.StreamReader] "C:\path\to\your\file.txt"
try {
    while ($line = $stream.ReadLine()) {
        Write-Output $line
    }
}
finally {
    $stream.Close()
}
```

এটি আরও মেমোরি-দক্ষ পদ্ধতি, বিশাল টেক্সট পাহাড়ের জন্য উপকারী।

বিকল্প? ভালো, আপনি CSV ফাইলগুলির জন্য `Import-Csv` বা JSON এর জন্য `ConvertFrom-Json` ব্যবহার করতে পারেন, যদি আপনি ডেটা গঠনমূলক বস্তুতে স্থানান্তর করতে চান। কিন্তু কাঁচা টেক্সট জিনিসের জন্য `Get-Content` এ স্থির থাকুন।

## দেখে নিন
আরও ধনমাল জানতে অফিশিয়াল ডকুমেন্টস দেখুন:

- [Get-Content ডকুমেন্টেশন](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [অটোম্যাটিক ভ্যারিয়েবলস সম্পর্কে](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables) - এটি ভ্যারিয়েবলগুলি সম্পর্কে অন্তর্দৃষ্টি দেয়, যেমন `$_`, যা ইনলাইন প্রক্রিয়াজাতকরণের জন্য সুবিধাজনক হতে পারে।
- [PowerShell এর .NET ক্ষমতাগুলি ব্যবহার করা](https://docs.microsoft.com/en-us/powershell/scripting/developer/hosting/adding-and-invoking-commands?view=powershell-7.1) - PowerShell এর মধ্যে .NET ফ্রেমওয়ার্ক গভীরে ডুব দেওয়ার জন্য।
