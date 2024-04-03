---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:24:03.070905-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: PowerShell \u099A\u09BE\u09B2\u09C1\
  \ \u0995\u09B0\u09C1\u09A8 \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF REPL-\u098F\
  \ \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09C1\u09A8\u0964 \u099F\u09CD\
  \u09B0\u09BE\u0987 \u0995\u09B0\u09C1\u09A8 `Get-Date` Cmdlet."
lastmod: '2024-03-17T18:47:44.277527-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09C1\u09A8 \u098F\u09AC\
  \u0982 \u0986\u09AA\u09A8\u09BF REPL-\u098F \u09AA\u09CD\u09B0\u09AC\u09C7\u09B6\
  \ \u0995\u09B0\u09C1\u09A8\u0964 \u099F\u09CD\u09B0\u09BE\u0987 \u0995\u09B0\u09C1\
  \u09A8 `Get-Date` Cmdlet."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
PowerShell চালু করুন এবং আপনি REPL-এ প্রবেশ করুন। ট্রাই করুন `Get-Date` Cmdlet:

```PowerShell
PS > Get-Date
```

আপনি বর্তমান তারিখ এবং সময়ের আউটপুট দেখতে পাবেন:

```PowerShell
বুধবার, মার্চ ৩১, ২০২৩ ১২:৩৪:৫৬ অপরাহ্ণ
```

এখন, কমান্ডস চেইন করুন। চলুন প্রসেসগুলিকে স্মৃতি ব্যবহার অনুসারে সাজাই:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

এটি শীর্ষ ৫ প্রসেসের আউটপুট দেয় যা কাজের সেটের আকারে (স্মৃতি ব্যবহারে) রয়েছে।

## গভীর ডুব
PowerShell-এর REPL-এর মূল রয়েছে ইউনিক্স শেল এবং অন্যান্য ডাইনামিক ভাষার শেলগুলি, যেমন পাইথন-এর, এর মধ্যে। এটি একজন একক ব্যবহারকারীর, ইন্টারেক্টিভ কমান্ড এক্সিকিউশন পরিবেশ। একটি কম্পাইল ভাষার মতো যেখানে আপনি সম্পূর্ণ অ্যাপ্লিকেশন লিখেন এবং তারপর কম্পাইল করেন, একটি REPL পরিবেশ আপনাকে এক লাইন কোড এক সময় লিখতে এবং চালাতে দেয়। PowerShell বড় কাজের জন্য স্ক্রিপ্ট এক্সিকিউশনও সমর্থন করে।

উইন্ডোজের জন্য বিকল্পগুলো হল কমান্ড প্রম্পট অথবা IPython এর মতো ভাষা-নির্দিষ্ট REPLs। ইউনিক্স/লিনাক্স বিশ্বে, bash অথবা zsh এর মতো শেলগুলি একই কাজ সম্পাদন করে।

PowerShell-এর বাস্তবায়ন শেল চালানোর জন্য একটি হোস্ট অ্যাপ্লিকেশন ব্যবহার করে। যদিও উইন্ডোজে PowerShell.exe সবচেয়ে প্রচলিত, অন্যান্যগুলি যেমন ইন্টিগ্রেটেড স্ক্রিপ্টিং এনভায়রনমেন্ট (ISE) বা ভিজ্যুয়াল স্টুডিও কোডের ইন্টিগ্রেটেড টার্মিনালও হোস্ট হিসেবে কাজ করতে পারে।

## দেখুন এছাড়াও
- [PowerShell সম্পর্কে](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
