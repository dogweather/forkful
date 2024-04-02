---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:51.418318-06:00
description: "PowerShell \u098F \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF `DateTime` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0995\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09CD\u09B0\u09A6\u09B0\
  \u09CD\u09B6\u09A8\u09C7\u09B0 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u2026"
lastmod: '2024-03-17T18:47:44.288871-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u098F \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\
  \u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF `DateTime` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u0995\u09C7\
  \ \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\
  \u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09CD\u09B0\u09A6\u09B0\
  \u09CD\u09B6\u09A8\u09C7\u09B0 \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u2026"
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কি এবং কেন?

PowerShell এ একটি তারিখকে স্ট্রিং এ রূপান্তর করা মানে একটি `DateTime` অবজেক্টকে টেক্সট ফরম্যাটে পরিবর্তন করা। প্রোগ্রামাররা এটি তারিখ প্রদর্শনের ফরম্যাট করা, লগের জন্য, ফাইলের নামে বা তথ্য সংরক্ষণ ও ডাটা ট্রান্সফারের জন্য সিরিয়ালাইজ করার লক্ষ্যে করে।

## কিভাবে:

একটি তারিখকে স্ট্রিং এ পরিণত করতে আমরা `ToString` মেথড বা `-f` ফরম্যাট অপারেটর ব্যবহার করি। এই প্রকারে:

```PowerShell
# বর্তমান তারিখ এবং সময়
$date = Get-Date

# স্ট্রিং এ ডিফল্ট রূপান্তর
$dateString = $date.ToString()
Write-Output $dateString

# কাস্টম ফরম্যাট: বছর-মাস-দিন ঘন্টা:মিনিট
$customFormat = $date.ToString("yyyy-MM-dd HH:mm")
Write-Output $customFormat

# একই কাস্টম ফরম্যাটের জন্য -f অপারেটর ব্যবহার
$fString = "{0:yyyy-MM-dd HH:mm}" -f $date
Write-Output $fString
```

নমুনা আউটপুট:

```
2023-03-17 10:45:00
2023-03-17 10:45
2023-03-17 10:45
```

## গভীর ডাইভ

PowerShell, Unix shells এবং Windows Script Host দ্বারা অনুপ্রাণিত, ২০০৬ সালের দিকে তার প্রথম বিকাশের সময় `Get-Date` চালু করে। এটি তারিখ-সময় অপারেশনের জন্য প্রচলিত কমান্ড হয়ে ওঠে। `DateTime` অবজেক্টের উপর `ToString` মেথড এবং `-f` ফরম্যাট অপারেটর .NET থেকে ধার করা ধারণা গুলি, PowerShell কে তার অবজেক্ট-ওরিয়েন্টেড স্বাদ দান করে।

যদি `ToString()` কোনও ফরম্যাট নির্দিষ্ট না করা হয়, এটি বর্তমান সংস্কৃতির ফরম্যাটে পূর্ণ তারিখ এবং সময় বের করে দেয়। তবে যখন আপনার নির্দিষ্ট বিন্যাস দরকার, যেমন ISO 8601 বা কেবল দিন এবং মাস, তখন কাস্টম .NET তারিখ এবং সময় ফরম্যাট স্ট্রিং আপনার বন্ধু হয়ে ওঠে।

একটি অন্যান্য প্রাচীন পদ্ধতি আছে— `DateTime` এর ফরম্যাট প্যাটার্ন ব্যবহার করা, যেমন চার ডিজিটের বছরের জন্য `yyyy`, শূন্যে পূর্ণ মাসের জন্য `MM`। তারা অনেক বিন্যাস তৈরি করতে সহজবোধ্য এবং প্রচুর।

তারপরে ইউনিক্সে POSIX আছে, যেখানে `date` কমান্ডগুলি তাদের নিজস্ব ফরম্যাট স্পেসিফায়ারের সাথে রাজত্ব করে। PowerShell দুই পৃথিবীকে সেতুবন্ধন করে, পরিচিত পদ্ধতিগুলি গ্রহণ করে কিন্তু Windows সিস্টেমের সাথে গুরুতর সামঞ্জস্যতা সরবরাহ করে।

বিকল্পগুলো অন্তর্ভুক্ত করে তারিখের উপাদানগুলোর বেসিক যোগফল এবং বাহ্যিক ইউটিলিটি বা ভাষা ইনফ্রাস্ট্রাকচারের ব্যবহার। তবে, PowerShell তার নিজস্ব রোবাস্ট নেটিভ কমান্ডগুলির সাথে জিনিসগুলি অভ্যন্তরীণভাবে রাখতে পছন্দ করে।

অফিসিয়াল Microsoft ডকুমেন্টেশনে ফরম্যাট স্পেসিফায়ারের গভীরে ডুব দিতে পারেন বা কমিউনিটি-লিখিত ব্লগগুলো আবিষ্কার করতে পারেন যেগুলো প্রায়ই PowerShell এ তারিখ এবং সময়গুলি ম্যানিপুলেট করার সৃজনশীল উপায় ভাগ করে।

## আরও দেখুন

- [Get-Date](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date) cmdlet সম্পর্কে PowerShell এর অফিসিয়াল ডকুমেন্টেশন, যা ব্যবহার এবং উদাহরণ প্রদান করে।
- .NET এর মানক এবং কাস্টম [ফরম্যাট স্ট্রিং গাইড](https://docs.microsoft.com/dotnet/standard/base-types/standard-date-and-time-format-strings) গভীর ফর্ম্যাটিং বিস্তারিত বিবরণের জন্য।
- [PowerShell.org forums](https://powershell.org/forums/) বা [Stack Overflow](https://stackoverflow.com/questions/tagged/powershell+datetime) এর মত কমিউনিটি ব্লগগুলি বাস্তব-বিশ্বের উদাহরণ এবং সমাধানের আলোচনা শেয়ার করে।
