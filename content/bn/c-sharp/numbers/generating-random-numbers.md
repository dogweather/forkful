---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:05.056368-06:00
description: "C#-\u098F \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09A8\
  \u09AE\u09CD\u09AC\u09B0 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09B0\
  \u09BF\u09B8\u09C0\u09AE\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0985\u09A8\
  \u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u09B8\u09BE\u0982\u0996\u09CD\u09AF\u09BF\u0995\
  \ \u09AE\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F\u09CB\u0997\u09CD\u09B0\u09BE\u09AB\u09BF, \u09B8\u09BF\
  \u09AE\u09C1\u09B2\u09C7\u09B6\u09A8 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.033143-06:00'
model: gpt-4-0125-preview
summary: "C#-\u098F \u09B0\u200D\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09A8\u09AE\
  \u09CD\u09AC\u09B0 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09AA\u09B0\u09BF\
  \u09B8\u09C0\u09AE\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0985\u09A8\u09BF\
  \u09B6\u09CD\u099A\u09BF\u09A4 \u09B8\u09BE\u0982\u0996\u09CD\u09AF\u09BF\u0995\
  \ \u09AE\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u0987\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u0995\u09CD\u09B0\
  \u09BF\u09AA\u09CD\u099F\u09CB\u0997\u09CD\u09B0\u09BE\u09AB\u09BF, \u09B8\u09BF\
  \u09AE\u09C1\u09B2\u09C7\u09B6\u09A8 \u098F\u09AC\u0982 \u0997\u09C7\u09AE\u09BF\
  \u0982-\u098F\u09B0 \u09AE\u09A4\u09CB \u09AC\u09C8\u09B6\u09BF\u09B7\u09CD\u099F\
  \u09CD\u09AF \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09DF\u09A8\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7\u09A8 \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u0985\u09A8\u09BF\u09B6\u09CD\
  \u099A\u09BF\u09A4\u09A4\u09BE \u0985\u09A5\u09AC\u09BE \u09AC\u09BE\u09B8\u09CD\
  \u09A4\u09AC \u09AC\u09BF\u09B6\u09CD\u09AC\u09C7\u09B0 \u09B0\u200D\u09CD\u09AF\
  \u09BE\u09A8\u09CD\u09A1\u09AE\u09A8\u09C7\u09B8\u09C7\u09B0 \u0985\u09A8\u09C1\u0995\
  \u09B0\u09A3 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC\u0964\
  ."
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
weight: 12
---

## কিভাবে করতে হবে:
C# এ র‍্যান্ডম নম্বর উৎপন্ন করার জন্য সবচেয়ে সাধারণ উপায় হল `System.Random` ক্লাস ব্যবহার করা। এখানে এর ব্যবহার দেখানো একটি সহজ উদাহরণ দেওয়া হল:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // 1 থেকে 99 পর্যন্ত একটি সংখ্যা উৎপন্ন করে
        Console.WriteLine($"Random number: {randomNumber}");
    }
}
```

এটি একটি র‍্যান্ডম নাম্বার প্রদর্শন করবে যেমন:

```
Random number: 42
```

যদি 0.0 এবং 1.0 এর মধ্যে একটি র‍্যান্ডম ফ্লোটিং-পয়েন্ট নম্বর উৎপন্ন করতে চান, আপনি উত্স `NextDouble` পদ্ধতি ব্যবহার করতে পারেন:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Random double: {randomDouble}");
```

যদি আপনি এমন একটি নিরাপত্তা-সংবেদনশীল অ্যাপ্লিকেশনে কাজ করছেন যা ক্রিপ্টোগ্রাফিক র‍্যান্ডমনেস প্রয়োজন, তবে `System.Security.Cryptography` এ পাওয়া `RNGCryptoServiceProvider` ক্লাস ব্যবহার করা ভালো:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // একটি 4-বাইট লম্বা র‍্যান্ডম নম্বর তৈরি করে
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Cryptographically secure random number: {value}");
    }
}
```

## গভীরে গমন
C#-এ র‍্যান্ডম নম্বর উৎপাদন বছরের পর বছর বিকশিত হয়েছে। প্রাথমিকভাবে, পিউডো-র‍্যান্ডম নম্বর উৎপাদনের জন্য `System.Random` ক্লাস ছিল প্রধান পছন্দ। এটি পিউডো-র‍্যান্ডম কারণ, একটি নির্দিষ্ট সীড মান দেওয়ার পর, এটি একেই ধারাবাহিকতার মধ্যে একই সিরিজের সংখ্যা উৎপন্ন করবে, যা ডিবাগিং বা টেস্টের পুনরাবৃত্তিতে উপকারী হতে পারে।

মৌলিক চাহিদা পূরণের জন্য যথেষ্ট হলেও, `System.Random` থ্রেড-সেফ নয় এবং প্রাথমিক ফলাফল উৎপন্ন করতে পারে, যা নিরাপত্তা-নির্ভর অ্যাপ্লিকেশনের জন্য উপযুক্ত নয়। এই সীমাবদ্ধতা ক্রিপ্টোগ্রাফিক র‍্যান্ডমনেসের জন্য `RNGCryptoServiceProvider` এর প্রবর্তন ঘটেছে, যা আরও নিরাপদ কিন্তু সম্পদ ব্যয়বহুল।

.NET Core এবং .NET 5+ এর একটি বিকল্প হল `System.Security.Cryptography` এ `RandomNumberGenerator` ক্লাস, যা নিরাপদভাবে র‍্যান্ডম নম্বর উৎপন্ন করার জন্য একটি আধুনিক এবং ব্যবহারে সহজ বিকল্প হিসেবে চিন্তা করা হয়েছে।

C#-এ র‍্যান্ডম নম্বর উৎপাদনের প্রতিটি পদ্ধতির অ্যাপ্লিকেশনের চাহিদা অনুসারে তার জায়গা আছে। বেশিরভাগ অ্যাপ্লিকেশনে `System.Random` যথেষ্ট, তবে সেইসবের জন্য যা নিরাপদ, অপ্রত্যাশিত র‍্যান্ডম নম্বরের প্রয়োজন, ক্রিপ্টোগ্রাফিক ক্লাসগুলি একটি দৃঢ় বিকল্প সরবরাহ করে।
