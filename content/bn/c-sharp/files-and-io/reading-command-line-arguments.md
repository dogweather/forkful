---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:09:38.360142-06:00
description: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\
  \u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\u09BC\u09BE\
  \ \u098F\u0995\u099F\u09BF C# \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u0995\u09C7 \u09B2\u099E\u09CD\u099A \u09B8\u09AE\u09AF\u09BC\u09C7 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\
  \u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\
  \u09BF\u09AF\u09BC\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u0995\u09CB\u09A1 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:44.053617-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u09AA\u09A1\u09BC\u09BE \u098F\
  \u0995\u099F\u09BF C# \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u0995\
  \u09C7 \u09B2\u099E\u09CD\u099A \u09B8\u09AE\u09AF\u09BC\u09C7 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\
  \u09B0 \u0987\u09A8\u09AA\u09C1\u099F \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\
  \u09AF\u09BC\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0995\u09CB\
  \u09A1 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u09A8\u09BE \u0995\u09B0\
  \u09C7 \u0985\u09CD\u09AF\u09BE\u09AA\u09C7\u09B0 \u0986\u099A\u09B0\u09A3 \u0995\
  \u09BE\u09B8\u09CD\u099F\u09AE\u09BE\u0987\u099C \u0995\u09B0\u09C7\u0964."
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
weight: 23
---

## কি এবং কেন?
কমান্ড লাইন আর্গুমেন্ট পড়া একটি C# প্রোগ্রামকে লঞ্চ সময়ে দেওয়া ব্যবহারকারীর ইনপুট প্রক্রিয়া করতে দেয়। প্রোগ্রামাররা এটি ব্যবহার করে কোড পরিবর্তন না করে অ্যাপের আচরণ কাস্টমাইজ করে।

## কিভাবে:
এখানে কমান্ড লাইন আর্গুমেন্টগুলি গিলতে কিভাবে:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("আপনি নিম্নলিখিত আর্গুমেন্টগুলি প্রবেশ করেছেন:");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

যদি আপনি আপনার প্রোগ্রামটি এইরকম চালান: `yourapp.exe arg1 arg2 arg3`, তাহলে নিম্নলিখিত আউটপুট প্রত্যাশা করুন:

```
আপনি নিম্নলিখিত আর্গুমেন্টগুলি প্রবেশ করেছেন:
arg1
arg2
arg3
```

## গভীর অনুসন্ধান
কমান্ড লাইন আর্গুমেন্টের প্রথা কম্পিউটিংয়ের ভোরে ফিরে যায়, প্রাথমিক সফটওয়্যারকে নমনীয় করে তোলে। C#-এ, `args` হলো `Main()`-এ পাস করা আর্গুমেন্টগুলি ধারণকারী একটি স্ট্রিং অ্যারে। বিকল্প? অবশ্যই, `CommandLineParser` এর মতো লাইব্রেরিগুলি রয়েছে যা ক্ষমতাগুলি বৃদ্ধি করে, কিন্তু অনেক কাজের জন্য, `args` আপনার দ্রুত এবং খারাপ বন্ধু।

ভিতরের দিকে, C# অ্যাপটি `Main()` দিয়ে শুরু হয়। যখন আপনি একটি কমান্ড লাইন বা স্ক্রিপ্ট থেকে আপনার অ্যাপে কল করেন, অপারেটিং সিস্টেমটি আর্গুমেন্টগুলিকে একটি অ্যারেতে প্যাক করে এবং এটি `Main()`-এ পাস করে। খুবই সহজ।

আপনার কি একটি জটিল অ্যাপ আছে? হয়তো আপনার প্রয়োজন পতাকা, অপশন এবং মান পার্স করা? এখানেই লাইব্স নিজেদের আরো নিয়ন্ত্রণ এবং কাঁচা `args` পার্সিং থেকে কম বয়লারপ্লেট কোডের সাথে উজ্জ্বল হয়। কিন্তু সহজ ইনপুটের জন্য? `args` সর্বদা।

## আরও দেখুন
- [Microsoft Docs এ Main() এবং কমান্ড-লাইন আর্গুমেন্টস সম্পর্কিত](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/program-structure/main-command-line)
- [GitHub এ CommandLineParser লাইব্রেরি](https://github.com/commandlineparser/commandline)
- [C#-এ কমান্ড লাইন আর্গুমেন্টগুলি পার্সিং করার উপর Stack Overflow আলোচনা](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
