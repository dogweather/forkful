---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:35.993716-06:00
description: ''
lastmod: '2024-04-05T22:51:05.509083-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কীভাবে:


### বর্তমান তারিখের সাথে দিন যোগ করা:
```PowerShell
# আজকের তারিখে 10 দিন যোগ
$newDate = (Get-Date).AddDays(10)
Write-Output $newDate
```

নমুনা আউটপুট:
```
বৃহস্পতিবার, এপ্রিল 13, 2023
```

### বর্তমান তারিখ থেকে দিন বাদ দেওয়া:
```PowerShell
# আজকের দিন থেকে 15 দিন বাদ
$pastDate = (Get-Date).AddDays(-15)
Write-Output $pastDate
```

নমুনা আউটপুট:
```
বুধবার, মার্চ 20, 2023
```

### দুটি তারিখের মধ্যে পার্থক্য নির্ণয় করা:
```PowerShell
# দুটি তারিখের পার্থক্য
$date1 = Get-Date '2023-04-01'
$date2 = Get-Date '2023-04-15'
$diff = $date2 - $date1
Write-Output $diff.Days
```

নমুনা আউটপুট:
```
14
```

## গভীরে ডুব দেওয়া
এক সময়, প্রোগ্রামারদের জটিল অ্যালগরিদম ব্যবহার করে ম্যানুয়ালি তারিখ নির্ণয় করতে হত। এখন, PowerShell এর মতো ভাষাগুলো `AddDays`, `AddMonths` মতো অন্তর্নির্মিত ফাংশন সরবরাহ করে, যা প্রায় সরলীকৃত।

### বিকল্পগুলি:
`AddDays` সাহায্যকারী হলেও, আরো সূক্ষ্ম নিয়ন্ত্রণের জন্য `AddHours`, `AddMinutes`, ইত্যাদি ফাংশন রয়েছে। পাশাপাশি, যদি আপনি স্ট্যাটিক পদ্ধতি পছন্দ করেন তবে `[datetime]::Today.AddDays(10)` ব্যবহার করতে পারেন।

### বাস্তবায়নের বিস্তারিত:
PowerShell-এর `DateTime` অবজেক্টে এই পদ্ধতিগুলি অন্তর্নির্মিত হয়েছে, সুতরাং আপনাকে নতুন করে চাকা আবিষ্কার করতে হবে না। অভ্যন্তরীণভাবে, এটি অধিবর্ষ এবং দিবাসঞ্চয়ন সময় সামঞ্জস্যের মতো জটিলতাগুলি আপনার পক্ষে সামলাচ্ছে।

## দেখুন আরও
- `DateTime` পদ্ধতিগুলি সম্পর্কে PowerShell-এর অফিসিয়াল ডকুমেন্টেশন: [Microsoft Docs - DateTime Methods](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- PowerShell তারিখ অঙ্কগণিত সম্পর্কে আরও: [PowerShell Date Arithmetic](https://ss64.com/ps/syntax-dateformats.html)
- তারিখ গণনার সংশ্লিষ্ট ক্যালেন্ডার সিস্টেমগুলির ইতিহাস এবং জটিলতা ডুব দিতে: [The Calendar FAQ](http://www.tondering.dk/claus/cal/calendar29.html)
