---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:43:08.241328-06:00
description: "PowerShell-\u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AC\u09BE\u09B0\u09CD\
  \u09A4\u09BE \u09AC\u09BE \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\
  \u09BF\u0995\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\
  \u09BF stderr \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB, \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout)\u2026"
lastmod: '2024-03-17T18:47:44.294738-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AC\u09BE\u09B0\u09CD\
  \u09A4\u09BE \u09AC\u09BE \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\
  \u09BF\u0995\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09B0\u09BE\u09B8\u09B0\
  \u09BF stderr \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB, \u09AF\u09BE \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout)\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি এবং কেন?

PowerShell-এ স্ট্যান্ডার্ড এরর (stderr) এ লেখা মানে হল এরর বার্তা বা ডায়াগনস্টিকসগুলিকে সরাসরি stderr স্ট্রিমে পাঠানো, যা স্ট্যান্ডার্ড আউটপুট (stdout) স্ট্রিম থেকে আলাদা। এই পৃথকীকরণটি একটি স্ক্রিপ্টের আউটপুটের উপর আরও নির্দিষ্ট নিয়ন্ত্রণ প্রদান করে, যা ডেভেলপারদের সাধারণ এবং এরর বার্তাগুলিকে বিভিন্ন গন্তব্যে পরিচালনা করতে সক্ষম করে, যা এরর হ্যান্ডলিং এবং লগিং এর জন্য মৌলিক।

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
