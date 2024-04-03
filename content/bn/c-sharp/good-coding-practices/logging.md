---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:43.196415-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C# \u098F, \u0986\u09AA\u09A8\u09BF\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 `System.Diagnostics` \u09A8\u09C7\u09AE\
  \u09B8\u09CD\u09AA\u09C7\u09B8 \u0985\u09A5\u09AC\u09BE NLog \u09AC\u09BE log4net\
  \ \u098F\u09B0 \u09AE\u09A4 \u09A5\u09BE\u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\
  \u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 .NET Core \u098F\u2026"
lastmod: '2024-03-17T18:47:44.043988-06:00'
model: gpt-4-0125-preview
summary: "C# \u098F, \u0986\u09AA\u09A8\u09BF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\
  \u09A8 `System.Diagnostics` \u09A8\u09C7\u09AE\u09B8\u09CD\u09AA\u09C7\u09B8 \u0985\
  \u09A5\u09AC\u09BE NLog \u09AC\u09BE log4net \u098F\u09B0 \u09AE\u09A4 \u09A5\u09BE\
  \u09B0\u09CD\u09A1-\u09AA\u09BE\u09B0\u09CD\u099F\u09BF \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 .NET Core \u098F \u09AA\u09BE\u0993\u09AF\u09BC\u09BE `ILogger`\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AB\u09C7\u09B8 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\
  \u09BE \u09B9\u09B2\u0983."
title: "\u09B2\u0997\u09BF\u0982"
weight: 17
---

## কিভাবে:
C# এ, আপনি বিল্ট-ইন `System.Diagnostics` নেমস্পেস অথবা NLog বা log4net এর মত থার্ড-পার্টি লাইব্রেরিগুলি ব্যবহার করতে পারেন। এখানে .NET Core এ পাওয়া `ILogger` ইন্টারফেস ব্যবহার করে একটি দ্রুত উদাহরণ দেওয়া হলঃ

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("এটি একটি তথ্যমূলক বার্তা।");
        logger.LogWarning("এটি একটি সতর্কতা বার্তা।");
        logger.LogError("এটি একটি ত্রুটি বার্তা।");
    }
}
```

নমুনা আউটপুট:
```
info: Program[0]
      এটি একটি তথ্যমূলক বার্তা।
warn: Program[0]
      এটি একটি সতর্কতা বার্তা।
fail: Program[0]
      এটি একটি ত্রুটি বার্তা।
```

## গভীর ডুব
সফটওয়্যার ডেভেলপমেন্টে লগিং এর ইতিহাস প্রায় প্রোগ্রামিং নিজের মতোই পুরোনো; এটি সাধারণ প্রিন্ট স্টেটমেন্টগুলি থেকে শুরু করে জটিল, কনফিগারযোগ্য সিস্টেমে বিকশিত হয়েছে। মূলত, লগিং কনসোল বা ফাইলগুলিতে লেখা হত, কিন্তু এটি লগ এগ্রিগেশন সিস্টেম এবং ডিস্ট্রিবিউটেড ট্রেসিং প্ল্যাটফর্মগুলির (যেমন ELK স্ট্যাক বা Jaeger) মতো আরও জটিল কাঠামোতে বেড়ে উঠেছে।

.NET এর বিল্ট-ইন লগিংয়ের বিকল্পগুলি হল থার্ড-পার্টি লাইব্রেরিগুলি:
- **NLog**: বহুমুখী এবং সেট আপ করা সহজ, লগ রাউটিং, ফরম্যাটিং, এবং ফিল্টারিংয়ের অনেক বৈশিষ্ট্য সহ।
- **log4net**: জাভা log4j লাইব্রেরি অনুপ্রাণিত, এটি XML থেকে অত্যন্ত কনফিগারযোগ্য এবং এটি বিভিন্ন লগ রেপোজিটরিগুলি সমর্থন করে।

লগিং অ্যাবস্ট্রাকশনের (যেমন Microsoft.Extensions.Logging) এর পছন্দ এবং অধীনস্থ লগিং প্রোভাইডারের বিস্তারিত বিবেচনা, আপনার অ্যাপ্লিকেশনের পারফরম্যান্স এবং নির্ভরতাকে গুরুত্বপূর্ণভাবে প্রভাবিত করতে পারে। লগিং লেভেলগুলি যথাযথভাবে কনফিগার করা এবং লগ রাইটিং একটি বোতলগলার মতো না হওয়া নিশ্চিত করা খুব জরুরী।

তাছাড়া, স্ট্রাকচারড লগিং - যেখানে আপনি শুধুমাত্র স্ট্রিং নয় বরং কী-ভ্যালু জোড়া বা অবজেক্ট লগ করেন - আরও প্রসারিত এবং কর্মপ্রেরণামূলক লগ অনুমোদন করে, যা জিজ্ঞাসা এবং বিশ্লেষণ করা সহজ।

## আরও দেখুন
- [Microsoft.Extensions.Logging ডকুমেন্টেশন](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [NLog ডকুমেন্টেশন](https://nlog-project.org/documentation/)
- [log4net ডকুমেন্টেশন](https://logging.apache.org/log4net/)
- [Serilog ডকুমেন্টেশন](https://serilog.net/) (স্ট্রাকচারড লগিং এর একটি উদাহরণের জন্য)
