---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:22.508621-06:00
description: "C# \u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\
  \u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE \u098F\
  \u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B0\u09C7\u0997\u09C1\u09B2\
  \u09BE\u09B0 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7\
  \ \u0986\u09B2\u09BE\u09A6\u09BE \u0995\u09B0\u09C7 \u098F\u09B0\u09B0 \u09AE\u09C7\
  \u09B8\u09C7\u099C \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\
  \u09CD\u099F\u09BF\u0995\u09B8 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\
  \u09B0\u09BE \u09B9\u09DF, \u09AF\u09C7\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0\u0995\u09BE\u09B0\u09C0\u09B0\u09BE \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.054734-06:00'
model: gpt-4-0125-preview
summary: "C# \u098F \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\
  \u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE \u098F\u09B0\
  \ \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09B0\u09C7\u0997\u09C1\u09B2\u09BE\
  \u09B0 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\
  \u09B2\u09BE\u09A6\u09BE \u0995\u09B0\u09C7 \u098F\u09B0\u09B0 \u09AE\u09C7\u09B8\
  \u09C7\u099C \u098F\u09AC\u0982 \u09A1\u09BE\u09DF\u09BE\u0997\u09A8\u09B8\u09CD\
  \u099F\u09BF\u0995\u09B8 \u09A8\u09BF\u09B0\u09CD\u09A6\u09C7\u09B6 \u0995\u09B0\
  \u09BE \u09B9\u09DF, \u09AF\u09C7\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \u0995\u09BE\u09B0\u09C0\u09B0\u09BE \u098F\u09AC\u0982\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কি এবং কেন?
C# এ স্ট্যান্ডার্ড এরর (stderr) এ লেখা এর মাধ্যমে রেগুলার আউটপুট (stdout) থেকে আলাদা করে এরর মেসেজ এবং ডায়াগনস্টিকস নির্দেশ করা হয়, যেন ব্যবহারকারীরা এবং ডেভেলপাররা সাধারণ প্রোগ্রাম আউটপুট এবং এরর নোটিফিকেশনের মধ্যে পার্থক্য করতে পারে। প্রোগ্রামাররা এটি করে ডিবাগিং এবং লগিং আরও দক্ষ করে তুলতে, যাতে অ্যাপ্লিকেশনের সঞ্চালন এবং রক্ষণাবেক্ষণ আরও সুচারু হয়।

## কিভাবে:
C# এ স্ট্যান্ডার্ড এররে লেখা যায় `Console.Error` স্ট্রিমের মাধ্যমে। এই স্ট্রিম বিশেষ করে এরর মেসেজ এবং ডায়াগনস্টিকসের জন্য ব্যবহৃত হয়। এখানে একটি বেসিক উদাহরণ দেওয়া হল:

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

স্যাম্পল আউটপুট (stderr এ):
```
Error: Failed to process the request.
```

যেসব পরিস্থিতিতে আপনি `Serilog` বা `NLog` এর মত থার্ড পার্টি লাইব্রেরি ব্যবহার করতে পারেন যেগুলো উন্নত লগিং ক্ষমতা প্রদান করে, আপনি এই লাইব্রেরিগুলি কনফিগার করতে পারেন যেন এরর লগগুলি stderr এ লেখা হয়। যদিও এই উদাহরণগুলি সিম্পল কনসোল রিডিরেকশনের উপর ফোকাস করে, মনে রাখবেন যে প্রোডাকশন অ্যাপ্লিকেশনে, লগিং ফ্রেমওয়ার্কগুলি অনেক বেশি সক্ষম এরর হ্যান্ডলিং এবং আউটপুট অপশন প্রদান করে। এখানে `Serilog` এর সাথে একটি সাধারণ উদাহরণ:

প্রথমে, Serilog প্যাকেজ এবং এর কনসোল সিঙ্ক ইনস্টল করুন:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

তারপর, Serilog কে stderr এ লেখার জন্য কনফিগার করুন:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("This is a normal message.");
Log.Error("This is an error message.");
```

স্যাম্পল আউটপুট (stderr এরর মেসেজের জন্য):
```
[15:04:20 ERR] This is an error message.
```

মন্তব্য: Serilog এর কনসোল সিঙ্কে `standardErrorFromLevel` কনফিগারেশন নির্দেশিত লেভেলে (এই ক্ষেত্রে এরর) বা উচ্চতর সব লগ ইভেন্টকে স্ট্যান্ডার্ড এরর স্ট্রিমে পুনঃনির্দেশ করে, অন্যদিকে নিম্ন স্তরের মেসেজগুলি যেমন ইনফরমেশন স্ট্যান্ডার্ড আউটপুট স্ট্রিমে লেখা হয়।
