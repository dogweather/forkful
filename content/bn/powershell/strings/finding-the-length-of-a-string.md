---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:56.785579-06:00
description: "PowerShell-\u098F, \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982-\u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\
  \u09CB\u099C\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09AF\u09C7 \u09AA\
  \u09B0\u09BF\u09AE\u09BE\u09A3 \u0985\u0995\u09CD\u09B7\u09B0 \u09A7\u09BE\u09B0\
  \u09A3 \u0995\u09B0\u09C7 \u09A4\u09BE \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0987\u09A8\u09AA\u09C1\u099F \u09AF\u09BE\u099A\u09BE\u0987, \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.261997-06:00'
model: gpt-4-0125-preview
summary: "PowerShell-\u098F, \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982-\u098F\u09B0 \u09A6\u09C8\u09B0\u09CD\u0998\u09CD\u09AF \u0996\u09CB\
  \u099C\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09AF\u09C7 \u09AA\u09B0\
  \u09BF\u09AE\u09BE\u09A3 \u0985\u0995\u09CD\u09B7\u09B0 \u09A7\u09BE\u09B0\u09A3\
  \ \u0995\u09B0\u09C7 \u09A4\u09BE \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u0987\u09A8\u09AA\u09C1\u099F \u09AF\u09BE\u099A\u09BE\u0987, \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u098F\u09AC\u0982\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u0996\u09C1\u0981\u099C\u09C7 \u09AA\u09BE\u0993\u09AF\
  \u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
PowerShell-এ, একটি স্ট্রিং-এর দৈর্ঘ্য খোজা মানে এটি যে পরিমাণ অক্ষর ধারণ করে তা গণনা করা। প্রোগ্রামাররা ইনপুট যাচাই, টেক্সট ডেটা ম্যানিপুলেট করা এবং নিশ্চিত করা যে ডেটা নির্দিষ্ট ক্রিটেরিয়া বা ফর্ম্যাটে ফিট বসে তার জন্য এটি করে।

## কিভাবে:
PowerShell-এ স্ট্রিং দৈর্ঘ্য পাওয়া সরাসরি সম্ভব। শুধু একটি স্ট্রিং `.Length` প্রোপার্টির কাছে নিক্ষেপ করুন, এইরকম ভাবে:

```PowerShell
$myString = "হ্যালো, ওয়ার্ল্ড!"
$myStringLength = $myString.Length
Write-Host "স্ট্রিং-এর দৈর্ঘ্য: $myStringLength"
```

আপনি আউটপুট পাবেন:

```
স্ট্রিং-এর দৈর্ঘ্য: 13
```

এটি সব আছে। সরাসরি এবং নির্বিঘ্ন।

## গভীর ডাইভ
পুরানো দিনে, অধিকাংশ প্রোগ্রামিং ভাষায় একটি স্ট্রিং-এর দৈর্ঘ্য পেতে জটিল ফাংশন বা প্রক্রিয়া জড়িত হত। আজকাল, PowerShell-এ এটি শুধুমাত্র একটি প্রোপার্টি কল হিসেবে সহজ।

মৌলিক `.Length` প্রোপার্টির বাইরে, PowerShell এই নির্দিষ্ট টাস্কের জন্য অন্তর্নির্মিত বিকল্প প্রদান করে না। তবে, PowerShell আসার আগে, উইন্ডোজে স্ক্রিপ্টিং ব্যাচ ফাইল বা VBScript-এর মাধ্যমে করা হত, যেখানে একটি স্ট্রিং-এর দৈর্ঘ্য খুঁজে পাওয়া এত সহজ ছিল না।

বাস্তবায়নের দিক থেকে, যখন আপনি `$myString.Length` ব্যবহার করেন, PowerShell স্ট্রিং অবজেক্টের মেটাডেটা অ্যাক্সেস করে - PowerShell-এ স্ট্রিংগুলি .NET থেকে আসা System.String ক্লাসের অবজেক্ট। `.Length` প্রোপার্টি সেই ক্লাসের একটি সদস্য।

## আরও দেখুন
PowerShell স্ট্রিংগুলিতে আরও গভীরে ডুব দিন:

.NET-এ স্ট্রিংগুলি কিভাবে কাজ করে তার বৃহত্তর প্রেক্ষাপটের জন্য:
- [.NET-এ স্ট্রিং ক্লাস](https://docs.microsoft.com/dotnet/api/system.string)
- [.NET-এ স্ট্রিং.Length প্রোপার্টি](https://docs.microsoft.com/dotnet/api/system.string.length)
