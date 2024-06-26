---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:53.413113-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 C# \u098F\
  \ \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\
  \u09B0\u09C7 \u09A6\u09C7\u0996\u09BF\u0964 \u09A7\u09B0\u09BE \u09AF\u09BE\u0995\
  , \u0986\u09AE\u09BE\u09A6\u09C7\u09B0 \u0995\u09BE\u099B\u09C7 \u09A6\u09C1\u099F\
  \u09BF `DateTime` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u0986\u099B\u09C7\
  , `date1` \u098F\u09AC\u0982 `date2`\u0964 \u0986\u09AE\u09B0\u09BE `DateTime.Compare(date1,\
  \ date2)`,\u2026"
lastmod: '2024-03-17T18:47:44.050528-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 C# \u098F \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09C7 \u09A6\u09C7\u0996\u09BF\u0964\
  \ \u09A7\u09B0\u09BE \u09AF\u09BE\u0995, \u0986\u09AE\u09BE\u09A6\u09C7\u09B0 \u0995\
  \u09BE\u099B\u09C7 \u09A6\u09C1\u099F\u09BF `DateTime` \u0985\u09AC\u099C\u09C7\u0995\
  \u09CD\u099F \u0986\u099B\u09C7, `date1` \u098F\u09AC\u0982 `date2`\u0964 \u0986\
  \u09AE\u09B0\u09BE `DateTime.Compare(date1, date2)`, `date1.CompareTo(date2)`, \u0985\
  \u09A5\u09AC\u09BE \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF properties \u098F\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09C7 \u098F\u0987 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BF."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
চলুন C# এ তারিখের তুলনা করে দেখি। ধরা যাক, আমাদের কাছে দুটি `DateTime` অবজেক্ট আছে, `date1` এবং `date2`। আমরা `DateTime.Compare(date1, date2)`, `date1.CompareTo(date2)`, অথবা সরাসরি properties এর তুলনা করে এই তুলনা করি:

```C#
DateTime date1 = new DateTime(2023, 3, 25);
DateTime date2 = new DateTime(2023, 3, 30);

// DateTime.Compare স্ট্যাটিক মেথড ব্যবহার করা
int result = DateTime.Compare(date1, date2);

if(result < 0)
    Console.WriteLine("date1 হল date2 এর তুলনায় আগের।");
else if(result == 0)
    Console.WriteLine("date1 এবং date2 সমান।");
else
    Console.WriteLine("date1 হল date2 এর তুলনায় পরবর্তী।");

// CompareTo ইন্সট্যান্স মেথড ব্যবহার করা
result = date1.CompareTo(date2);

if(result < 0)
    Console.WriteLine("date1 আবারও আগে।");
else if(result == 0)
    Console.WriteLine("এখনও একই সময়?");
else
    Console.WriteLine("এইবার কি date1 দেরি করে এসেছে?");

// সরাসরি তুলনা
if(date1 < date2)
    Console.WriteLine("হ্যাঁ, দেখা যাচ্ছে date1 আগের, সরাসরি।");
else if(date1 == date2)
    Console.WriteLine("সমান, সহজ এবং সরল।");
else
    Console.WriteLine("নাকি date1 পরে? না, এই বার না।");
```

আউটপুট দেখাবে যে `date1` সমস্ত তুলনায় `date2` এর তুলনায় আগে—আপনি স্পষ্ট কিছু বলছেন, তবে লগগুলোর জন্য এটা জরুরী।

## গভীর ডাইভ
DateTime তুলনাগুলি C# এর শুরু থেকে এর এক অপরিহার্য অংশ, সময়ের এই অত্যন্ত গুরুত্বপূর্ণ ধারণার সাথে কাজ করার জন্য। অভ্যন্তরীণভাবে, `DateTime` মানগুলি মধ্যরাত, জানুয়ারী 1, 0001 থেকে টিক গণনা করে।

বিকল্প খুঁজছেন? আপনি `TimeSpan` ব্যবহার করে পার্থক্যের জন্য, অথবা Jon Skeet দ্বারা নির্মিত NodaTime এর সাথে যেতে পারেন, যা আরও জটিল তারিখ এবং সময় হ্যান্ডলিংয়ের জন্য একটি লাইব্রেরি।

একটি প্রযুক্তিগত মজাদার তথ্য: .NET এ `DateTime` ধরণের হতে পারে `Unspecified`, `Utc`, অথবা `Local`। একটি UTC সময়ের সাথে একটি লোকাল সময়ের তুলনা? এটি সমস্যার জন্য আহ্বান। সবসময় নিশ্চিত করুন যে ধরণের মিল আছে যাতে যুক্তি বিশৃঙ্খল না হয়!

## আরও দেখুন
এগুলোর সাথে গভীরভাবে ঘুরে দেখুন বা জিনিসগুলো স্পষ্ট করুন:

- Microsoft's DateTime ডকুমেন্টেশন: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- DateTime.Kind জানুন: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.kind
- কৌতুহলী ঘড়ি প্রেমীদের জন্য NodaTime: https://nodatime.org/
- সময়ের পার্থক্যের জন্য TimeSpan: https://docs.microsoft.com/en-us/dotnet/api/system.timespan
