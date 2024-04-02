---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:02.969992-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE\
  \ \u0985\u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\
  \u09B8\u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF \u0985\u09A8\
  \u09CD\u09A4\u09B0 \u09AA\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u0986\u0997\u09C7\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u099F\u09BF \u0995\u09BF \u099B\u09BF\u09B2, \u0985\
  \u09A5\u09AC\u09BE \u0995\u09BF \u09B9\u09AC\u09C7, \u09A4\u09BE \u09A8\u09BF\u09B0\
  \u09CD\u09A3\u09DF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09DF\u0987\
  \ \u098F\u0987 \u09A4\u09A5\u09CD\u09AF\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.051517-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u0985\u09A5\u09AC\u09BE \u0985\
  \u09A4\u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u09B9\u09BF\u09B8\
  \u09C7\u09AC \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u0995 \u09A8\u09BF\
  \u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09DF \u0985\u09A8\u09CD\
  \u09A4\u09B0 \u09AA\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u0986\u0997\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996\u099F\u09BF \u0995\u09BF \u099B\u09BF\u09B2, \u0985\u09A5\
  \u09AC\u09BE \u0995\u09BF \u09B9\u09AC\u09C7, \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\
  \u09A3\u09DF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09DF\u0987 \u098F\
  \u0987 \u09A4\u09A5\u09CD\u09AF\u09C7\u09B0\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি এবং কেন?

ভবিষ্যত অথবা অতীতের তারিখ হিসেব করা মানে এক নির্দিষ্ট সময় অন্তর পরে অথবা আগে তারিখটি কি ছিল, অথবা কি হবে, তা নির্ণয় করা। প্রোগ্রামাররা প্রায়ই এই তথ্যের প্রয়োজন পড়ে ইভেন্টগুলি নির্ধারণ, মেয়াদ শেষের তারিখ সামলানো, বা সময়-সংবেদনশীল ডেটা রেকর্ড করার কাজে।

## কিভাবে:

ভবিষ্যতের তারিখ হিসেব করা:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan oneWeek = TimeSpan.FromDays(7);
        
        DateTime nextWeek = currentDate + oneWeek;
        Console.WriteLine($"এখন থেকে এক সপ্তাহ পর: {nextWeek}");
    }
}
```

আউটপুট:

```
এখন থেকে এক সপ্তাহ পর: <বর্তমান তারিখ থেকে এক সপ্তাহ পরের তারিখ>
```

অতীতের তারিখ হিসেব করা:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan tenDaysAgo = TimeSpan.FromDays(-10);
        
        DateTime pastDate = currentDate + tenDaysAgo;
        Console.WriteLine($"দশ দিন আগে ছিল: {pastDate}");
    }
}
```

আউটপুট:

```
দশ দিন আগে ছিল: <বর্তমান তারিখ থেকে দশ দিন আগের তারিখ>
```

## গভীর ডুব

C#-এ, `DateTime` এবং `TimeSpan` হল তারিখ এবং সময় অপারেশনের জন্য মুখ্য উপকরণ। `DateTime` একটি মুহূর্তকে প্রকাশ করে, সাধারণত একটি তারিখ এবং দিনের সময় হিসেবে ব্যক্ত করা হয়। `TimeSpan` একটি সময় অন্তরালকে প্রকাশ করে।

ঐতিহাসিকভাবে, তারিখ এবং সময়ের হিসেবনিকেশে দিন, মাস এবং লিপ বছরের ম্যানুয়াল হ্যান্ডলিং এর ফলে ভুল হওয়ার সম্ভাবনা ছিল। `DateTime` এই জটিলতাগুলি সরলীকৃত করে, ফ্রেমওয়ার্ককে কঠিন অংশগুলি সামাল দিতে দেয়।

.NET-এ `DateTime` এবং `TimeSpan` এর বিকল্পগুলির মধ্যে `DateTimeOffset` রয়েছে, যা একটি সময় জোন অফসেট অন্তর্ভুক্ত করে, যার ফলে বিভিন্ন সময় অঞ্চলে কাজ করা অ্যাপ্লিকেশনগুলির জন্য এটি ভালো। অন্য একটি বিকল্প হল Noda Time, জন Skeet দ্বারা ডিজাইন করা একটি লাইব্রেরি যা বিভিন্ন ক্যালেন্ডারের মত আরও জটিল তারিখ এবং সময় হ্যান্ডলিং এর জন্য তৈরি।

বাস্তবায়নের দিক থেকে, যখন আপনি একটি `DateTime` এর সঙ্গে একটি `TimeSpan` যোগ করেন, অন্তর্নিহিতভাবে, এটি টিকের মাধ্যমে ম্যানিপুলেশন করে, .NET-এর সময়ের মৌলিক একক (`1 টিক = 100 ন্যানোসেকেন্ড`)। অতীতের তারিখের জন্য, একটি নেগেটিভ `TimeSpan` কাজে লাগে।

## আরও দেখুন

- .NET API ডকুমেন্টেশন এর [`DateTime`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [`TimeSpan`](https://docs.microsoft.com/en-us/dotnet/api/system.timespan) এর প্রবর্তন
- Microsoft এর [`DateTime` এবং `DateTimeOffset`](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime) এর জন্য সেরা অনুশীলন
- Noda Time ডকুমেন্টেশন: [https://nodatime.org](https://nodatime.org)
