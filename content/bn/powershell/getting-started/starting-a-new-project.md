---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:21:09.827914-06:00
description: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\
  \ \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09BF\u0982 \u09AE\u09BE\u09B8\u09CD\u099F\
  \u09BE\u09B0\u09AA\u09BF\u09B8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AD\u09BF\
  \u09A4\u09CD\u09A4\u09BF \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\u09C1\u09A4 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7, \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09BF \u09A8\u09A4\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09A7\u09BE\
  \u09B0\u09A3\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u09B6\u09C1\
  \u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.276061-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u0995\u09CB\u09A1\u09BF\u0982 \u09AE\u09BE\u09B8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09BF\u09B8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AD\u09BF\u09A4\
  \u09CD\u09A4\u09BF \u09AA\u09CD\u09B0\u09B8\u09CD\u09A4\u09C1\u09A4 \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7, \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09BF \u09A8\u09A4\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09A7\u09BE\
  \u09B0\u09A3\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u09B6\u09C1\
  \u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A5\u09AC\
  \u09BE \u09B8\u0982\u0997\u09A0\u09BF\u09A4, \u09B8\u09CD\u0995\u09C7\u09B2\u09AF\
  \u09CB\u0997\u09CD\u09AF \u0989\u09AA\u09BE\u09AF\u09BC\u09C7 \u09B8\u09AE\u09BE\
  \u09A7\u09BE\u09A8 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09AF\u09BC\u09A8\u09C7\
  \u09B0 \u099C\u09A8\u09CD\u09AF\u0964."
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কীভাবে:
PowerShell এর সাহায্যে নতুন প্রকল্প শুরু করা সরল। আপনি আপনার প্রকল্পের জন্য একটি ডিরেক্টরি তৈরি করতে এবং একটি গিট রিপোজিটরি সেট আপ করতে চাইতে পারেন। এখানে কিভাবে:

```PowerShell
# আপনার প্রকল্পের জন্য নতুন ডিরেক্টরি তৈরি করুন
New-Item -Path 'C:\MyProjects\NewCoolApp' -ItemType Directory

# আপনার নতুন ডিরেক্টরিতে নেভিগেট করুন
Set-Location -Path 'C:\MyProjects\NewCoolApp'

# যদি আপনি ভার্সন কন্ট্রোল ব্যবহার করে থাকেন তবে একটি নতুন গিট রিপোজিটরি ইনিশিয়ালাইজ করুন
git init
```

নমুনা আউটপুট:
```
    Directory: C:\MyProjects

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
d-----          1/1/2023   12:00 AM                NewCoolApp
Initialized empty Git repository in C:/MyProjects/NewCoolApp/.git/
```

## গভীর ডুব
PowerShell ২০০৬ সালে এর প্রথম আবির্ভাবের পর থেকে Windows অটোমেশনের জন্য পছন্দের স্ক্রিপ্টিং ভাষা হয়ে উঠেছে। PowerShell এর সাহায্যে নতুন প্রকল্প শুরু করা শুধু ডিরেক্টরি তৈরি করা নয়; এটি প্রকল্পের পরিসর নির্ধারণ, স্ক্রিপ্ট সংজ্ঞায়িত করা অথবা স্বয়ংক্রিয়কৃত কাজ প্রস্তুত করার একটি অনুষ্ঠান।

যদিও PowerShell Windows জগতে পছন্দের একটি, Unix-এর মত ব্যবহারকারীরা প্রায় 'bash' অথবা 'zsh' এর উপর নির্ভর করেন একই রকম কাজের জন্য। তবে, PowerShell Core এর আবির্ভাবের সাথে, PowerShell মাল্টিপ্ল্যাটফর্ম অঙ্গনে পা রেখেছে, আপোস-প্ল্যাটফর্ম স্ক্রিপ্টিং এবং অটোমেশন সম্ভব করেছে।

PowerShell এর ডিজাইনে গভীরে রয়েছে এর অবজেক্ট-ওরিয়েন্টেড প্রকৃতি, যা cmdlets (উচ্চারিত কমান্ড-লেটস্) ব্যবহার করে যা অবজেক্ট আউটপুট করে। `New-Item` এর মতো Cmdlets শুধু ফাইল বা ফোল্ডার তৈরি করে না; এরা অবজেক্ট গঠন করে যা আপনার স্ক্রিপ্টগুলি সাথে মিথষ্ক্রিয়া করতে পারে। নতুন প্রকল্প সেটআপে পারে ফোল্ডার স্ট্রাকচার প্রতিষ্ঠা, README তৈরি, .gitignore ফাইল সেটআপ, অথবা প্রারম্ভিক কোড ফাইলগুলির টেমপ্লেট তৈরি অন্তর্ভুক্ত থাকতে পারে।

PowerShell-এ প্রকল্প সেটআপের অনুশীলন বাস্তবায়ন ফাইল ম্যানিপুলেশন (`New-Item`) থেকে পরিবেশ কনফিগারেশন (`Set-Location`) পর্যন্ত বহু কমান্ডলেটস্ ব্যবহার করতে পারে। এগুলির সাথে PowerShell এর স্ক্রিপ্টিং ক্ষমতা মিলে শক্তিশালী সেটআপ স্ক্রিপ্ট তৈরি করতে পারে যা প্রকল্পের গাঁথুনি সামান্য ঝামেলায় তৈরি করে।

## দেখুন এছাড়াও
- [PowerShell স্ক্রিপ্টিং](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [Pro Git বই](https://git-scm.com/book/en/v2)
- [GitHub এর Hello World](https://guides.github.com/activities/hello-world/)
- [GitHub এ PowerShell Core](https://github.com/PowerShell/PowerShell)
