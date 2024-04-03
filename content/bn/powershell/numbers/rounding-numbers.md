---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:15:04.931902-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell-\u098F \u09B8\u0982\
  \u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\u0995\u09B0\u09A3\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1 \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\
  \u099C\u09A8\u0995 cmdlets \u098F\u09AC\u0982 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\
  \ \u09B0\u09AF\u09BC\u09C7\u099B\u09C7: - \u09AE\u09CD\u09AF\u09BE\u09A5 \u0995\u09CD\
  \u09B2\u09BE\u09B8 \u09A5\u09C7\u0995\u09C7 `Round()` \u09AA\u09A6\u09CD\u09A7\u09A4\
  \u09BF."
lastmod: '2024-03-17T18:47:44.266178-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2\
  \u0995\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09BF\u099B\u09C1\
  \ \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\u099C\u09A8\u0995 cmdlets \u098F\u09AC\u0982\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কিভাবে:
PowerShell-এ সংখ্যা গোলকরণের জন্য কিছু সুবিধাজনক cmdlets এবং পদ্ধতি রয়েছে:

- ম্যাথ ক্লাস থেকে `Round()` পদ্ধতি:
```PowerShell
[Math]::Round(15.68) # 16 এ গোলকরণ হয়
```
- দশমিক নির্দিষ্ট করা:
```PowerShell
[Math]::Round(15.684, 2) # 15.68 এ গোলকরণ হয়
```
- `Ceiling()` এবং `Floor()`, সবসময় উপরে বা নীচে গোলকরণ:
```PowerShell
[Math]::Ceiling(15.2) # 16 এ উপরে গোলকরণ করা হয়
[Math]::Floor(15.9) # 15 এ নিচে গোলকরণ করা হয়
```

## গভীরে প্রবেশ
সংখ্যা গোলকরণ নতুন কোনো ধারনা নয়; এটি প্রাচীন কাল থেকেই বাণিজ্য, বিজ্ঞান এবং সময় মাপার কাজে উপযোগী ছিল। PowerShell সম্পর্কে কথা বলতে গেলে, `[Math]::Round()` ডিফল্ট হিসেবে "ব্যাঙ্কার্স গোলকরণ" অনুসরণ করে, যেখানে 0.5স নিকটতম সম সংখ্যায় যায়, যা পরিসংখ্যানে পক্ষপাত কমায়।

`[Math]` পদ্ধতির সাথে আপনি শুধু আটকে নেই। আরো নিয়ন্ত্রণ চান? `[System.Math]::Round(সংখ্যা, দশমিক, MidpointRounding)` দেখুন যেখানে আপনি মিডপয়েন্ট কিভাবে সামলানো হবে তা সেট করতে পারবেন: শূন্য থেকে দূরে বা সমান (যা ব্যাঙ্কার্স গোলকরণ নামে পরিচিত)।

অন্য একটি দিক: `System.Globalization.CultureInfo` অবজেক্ট। এটি আন্তর্জাতিক সংখ্যাগুলি নিয়ে কাজ করার সময় স্থানীয়-নির্দিষ্ট ফরম্যাটিং এবং গোলকরণ পছন্দের সাথে সাহায্য করে।

## আরও দেখুন
- ম্যাথ পদ্ধতি সম্পর্কে মাইক্রোসফটের অফিসিয়াল ডক্স: [লিঙ্ক](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- .NET এ দশমিক গোলকরণের বিশেষ বিবরণ: [লিঙ্ক](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- StackOverflow এ গোলকরণ নিয়ে আলোচনা: [লিঙ্ক](https://stackoverflow.com/questions/tagged/rounding+powershell)
