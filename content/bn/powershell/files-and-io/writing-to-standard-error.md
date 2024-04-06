---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:08.241328-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell stderr \u098F \u09B2\
  \u09C7\u0996\u09BE\u09B0 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u099F\
  \u09BF `Write-Error` cmdlet \u09AC\u09BE `$host.ui.WriteErrorLine()` \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF\u09B0 \u0986\u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7\
  \ \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\u09C7 \u09B8\u09B9\u099C\
  \ \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u09A4\u09AC\u09C7, \u09B8\u09B0\
  \u09BE\u09B8\u09B0\u09BF\u2026"
lastmod: '2024-04-05T21:53:52.809982-06:00'
model: gpt-4-0125-preview
summary: "PowerShell stderr \u098F \u09B2\u09C7\u0996\u09BE\u09B0 \u09AA\u09CD\u09B0\
  \u0995\u09CD\u09B0\u09BF\u09DF\u09BE\u099F\u09BF `Write-Error` cmdlet \u09AC\u09BE\
  \ `$host.ui.WriteErrorLine()` \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u09B0 \u0986\u0989\
  \u099F\u09AA\u09C1\u099F\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\
  \ \u0995\u09B0\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\
  \u0964 \u09A4\u09AC\u09C7, \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF stderr \u09AA\u09C1\
  \u09A8\u0983\u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF, \u0986\u09AA\u09A8\u09BF .NET \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\
  \u0997\u09C1\u09B2\u09BF \u09AC\u09BE PowerShell \u09A8\u09BF\u099C\u09C7\u0987\
  \ \u09AA\u09CD\u09B0\u09A6\u09A4\u09CD\u09A4 \u09AB\u09BE\u0987\u09B2 \u09A1\u09C7\
  \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09B0 \u09AA\u09C1\u09A8\u0983\
  \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6\u09A8\u0995\u09C7 \u09AA\u099B\u09A8\u09CD\
  \u09A6 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 **\u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09E7:** `Write-Error` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 stderr \u098F \u098F\u0995\u099F\u09BF \u098F\u09B0\u09B0\
  \ \u09AE\u09C7\u09B8\u09C7\u099C \u09B2\u09C7\u0996\u09BE\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কিভাবে:
PowerShell stderr এ লেখার প্রক্রিয়াটি `Write-Error` cmdlet বা `$host.ui.WriteErrorLine()` পদ্ধতির আউটপুটকে নির্দেশ করে সহজ করে তোলে। তবে, সরাসরি stderr পুনঃনির্দেশনের জন্য, আপনি .NET পদ্ধতিগুলি বা PowerShell নিজেই প্রদত্ত ফাইল ডেস্ক্রিপ্টর পুনঃনির্দেশনকে পছন্দ করতে পারেন।

**উদাহরণ ১:** `Write-Error` ব্যবহার করে stderr এ একটি এরর মেসেজ লেখা।

```powershell
Write-Error "This is an error message."
```

stderr-এ আউটপুট:
```
Write-Error: This is an error message.
```

**উদাহরণ ২:** `$host.ui.WriteErrorLine()` ব্যবহার করে সরাসরি stderr লেখা।

```powershell
$host.ui.WriteErrorLine("Direct stderr write.")
```

stderr-এ আউটপুট:
```
Direct stderr write.
```

**উদাহরণ ৩:** stderr লেখার জন্য .NET পদ্ধতিগুলি ব্যবহার করা।

```powershell
[Console]::Error.WriteLine("Using .NET method for stderr")
```

এই পদ্ধতির আউটপুট:
```
Using .NET method for stderr
```

**উদাহরণ ৪:** ফাইল ডেস্ক্রিপ্টর `2>` ব্যবহার করে এরর আউটপুট পুনঃনির্দেশ করা।

PowerShell-এ ফাইল ডেস্ক্রিপ্টরগুলি বিভিন্ন স্ট্রিমগুলি পুনঃনির্দেশ করতে পারে। stderr-এর জন্য, ফাইল ডেস্ক্রিপ্টর হল `2`। এই উদাহরণে একটি এরর সৃষ্টিকারী কমান্ড চালানোর সময় `error.log` নামে একটি ফাইলে stderr পুনঃনির্দেশ করা দেখানো হয়েছে।

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

এই উদাহরণটি কনসল আউটপুট উৎপন্ন করে না, তবে বর্তমান ডিরেক্টরিতে `error.log` নামে একটি ফাইল তৈরি করে, যা বিদ্যমান নয় এমন একটি ফাইলে প্রবেশের চেষ্টার সময় থেকে আসা এরর বার্তাটি ধারণ করে।

সবশেষে, PowerShell এরর আউটপুট লেখা এবং পরিচালনা করার জন্য বহুবিধ পদ্ধতি প্রদান করে, যা স্ক্রিপ্ট এবং অ্যাপ্লিকেশনগুলিতে জটিল এরর হ্যান্ডলিং এবং লগিং কৌশল সম্ভব করে তোলে।
