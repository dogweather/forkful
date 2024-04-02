---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:43.831472-06:00
description: "PowerShell-\u098F \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u099F\u09BF \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE \u09AF\u09C7 \u098F\u0995\u099F\
  \u09BF \u0985\u09A8\u09CD\u09AF\u099F\u09BF\u09B0 \u099A\u09C7\u09AF\u09BC\u09C7\
  \ \u0986\u0997\u09C7, \u098F\u0995\u0987, \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\u09C7\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u0987\u09AD\u09C7\u09A8\u09CD\u099F\
  \ \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE, \u09B0\u09C7\u0995\u09B0\u09CD\
  \u09A1 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB, \u0995\u09BE\u099C\u2026"
lastmod: '2024-03-17T18:47:44.290294-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u099F\u09BF \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE \u09AF\u09C7 \u098F\u0995\u099F\
  \u09BF \u0985\u09A8\u09CD\u09AF\u099F\u09BF\u09B0 \u099A\u09C7\u09AF\u09BC\u09C7\
  \ \u0986\u0997\u09C7, \u098F\u0995\u0987, \u0985\u09A5\u09AC\u09BE \u09AA\u09B0\u09C7\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u0987 \u0987\u09AD\u09C7\u09A8\u09CD\u099F\
  \ \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE, \u09B0\u09C7\u0995\u09B0\u09CD\
  \u09A1 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB, \u0995\u09BE\u099C\u2026"
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কি এবং কেন?

PowerShell-এ দুটি তারিখ তুলনা করা মানে এটি বের করা যে একটি অন্যটির চেয়ে আগে, একই, অথবা পরে। প্রোগ্রামাররা প্রায়ই ইভেন্ট পরিচালনা, রেকর্ড সাজানো, কাজ নির্ধারণ, অথবা ডেটা আয়ু পরীক্ষা করতে এটি করে থাকেন।

## কিভাবে:

```PowerShell
# চলুন আজকের তারিখটি নেই
$today = Get-Date

# এবং এখানে একটি যাচ্ছেতাই তারিখ
$someOtherDate = Get-Date "2023-03-17"

# তারা সমান?
$today -eq $someOtherDate

# আজকে অন্য তারিখের থেকে বড় (পরে)?
$today -gt $someOtherDate

# এটি আগের কিনা পরীক্ষা করার ব্যাপারে কি?
$today -lt $someOtherDate

# ফলাফল দেখা যাক, কি বলেন?

মিথ্যা
সত্য
মিথ্যা
```

## গভীরে ডুব

কম্পিউটিং-এর পাথুরে যুগে—আসলে না, কিন্তু আপনি জানেন, প্রাথমিক দিনগুলিতে—তারিখগুলি জটিল ছিল। আমরা মানদণ্ড এবং PowerShell আরও সহজ করে তোলা দীর্ঘ পথ অতিক্রম করেছি।

মনে রাখার জন্য যে বিষয়গুলি:
1. **ইতিহাস**: কম্পিউটারগুলি বিভিন্ন ফরম্যাটে তারিখ সম্পন্ন করত, যা সম্ভাব্য বিভ্রান্তি ও Y2K-শৈলীর বাগ তৈরি করতে পারে। PowerShell এই ধরণের বিশৃঙ্খলা এড়াতে .NET-এর `DateTime` কাঠামোর উপর নির্ভর করে।

2. **বিকল্পগুলি**: আপনি `Compare-Object` ব্যবহার করতে পারেন, অথবা `[datetime]` অবজেক্টগুলি থেকে পদ্ধতিগুলি যেমন `.AddDays()` তুলনা করার আগে গণনা সম্পাদনের জন্য প্রয়োগ করতে পারেন। প্রদর্শন প্রভাব পরীক্ষা করতে `Measure-Command` মনে রাখুন।

3. **বাস্তবায়ন বিস্তারিত**: PowerShell তারিখগুলি তাদের নিজস্ব বৈশিষ্ট্য এবং পদ্ধতিগুলির সাথে অবজেক্ট। তারিখের তুলনা অপারেটর (`-eq`, `-lt`, `-gt`) দ্বারা করা হয়, এবং, অপারেটর অভিভারনের কারণে, PowerShell জানে আপনি তারিখ নিয়ে কাজ করছেন, শুধুমাত্র স্ট্রিং বা সংখ্যা নয়।

অ্যাসেম্বলি স্তরে, তারিখ তুলনা 1/1/0001 থেকে টিকগুলি (100-ন্যানোসেকেন্ড অন্তরাল) অনুবাদ করে। সুতরাং আপনি মূলত বড় পূর্ণসংখ্যা তুলনা করছেন, যা দক্ষ।

## দেখুন ও

- [DateTime কাঠামো (Microsoft ডক্স)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [PowerShell-এ তারিখ এবং সময় নিয়ে কাজ করা (SS64.com)](https://ss64.com/ps/syntax-dateformats.html)
