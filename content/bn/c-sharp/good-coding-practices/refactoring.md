---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:07.675401-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B8\u09C1\u09A8 \u0986\
  \u09AE\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \ C# \u09AE\u09C7\u09A5\u09A1 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u09B0 \u0995\u09B0\u09BF \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE\u09B0 \u0985\u09CD\u09AF\u09BE\u09B0\u09C7\u09B0 \u09AF\u09CB\
  \u0997\u09AB\u09B2 \u0997\u09A3\u09A8\u09BE \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\
  \u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09C7: \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\
  \u0995\u09CD\u099F\u09B0\u09BF\u0982 \u098F\u09B0 \u0986\u0997\u09C7."
lastmod: '2024-03-17T18:47:44.046327-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B8\u09C1\u09A8 \u0986\u09AE\u09B0\u09BE \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 C# \u09AE\u09C7\u09A5\u09A1 \u09B0\u09BF\u09AB\
  \u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\u09B0\u09BF \u09AF\u09BE \u098F\
  \u0995\u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u0985\u09CD\u09AF\
  \u09BE\u09B0\u09C7\u09B0 \u09AF\u09CB\u0997\u09AB\u09B2 \u0997\u09A3\u09A8\u09BE\
  \ \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09BF\u09A8\u09CD\u099F \u0995\u09B0\u09C7\
  ."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কিভাবে:
আসুন আমরা একটি সাধারণ C# মেথড রিফ্যাক্টর করি যা একটি সংখ্যার অ্যারের যোগফল গণনা এবং প্রিন্ট করে:

রিফ্যাক্টরিং এর আগে:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

রিফ্যাক্টরিং এর পরে:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// ব্যবহার:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

রিফ্যাক্টরিং এর মাধ্যমে, আমরা উদ্বেগ পৃথককরণ করেছি, `Calculator` ক্লাসকে আরও লচকদার করে তুলেছি যাতে এটি যেকোনো সংখ্যার অ্যারে নিতে পারে, এবং LINQ ব্যবহার করে যোগফল গণনা আরও সংক্ষিপ্ত করেছি।

## গভীর ডুব
রিফ্যাক্টরিং এর উৎস রয়েছে স্মলটক প্রোগ্রামিং কমিউনিটিতে এবং ১৯৯০ এর দশকে মার্টিন ফাউলারের বই "Refactoring: Improving the Design of Existing Code" দ্বারা এটি জনপ্রিয় হয়। বছরের পর বছর, এটি এজাইল পদ্ধতিগুলি এবং ভাল কোডিং অনুশীলনের একটি মৌলিক অংশ হয়ে উঠেছে।

রিফ্যাক্টরিং এর বিভিন্ন পদ্ধতি রয়েছে, যেমন টেস্ট-ড্রাইভেন ডেভেলপমেন্ট (TDD) এ রেড-গ্রিন-রিফ্যাক্টর। এটি নিশ্চিত করে যে রিফ্যাক্টরিং বাগ প্রবর্তন করে না একটি ব্যর্থ টেস্ট দিয়ে শুরু করে, এটিকে পাস করা, এবং তারপর কোডটি পরিষ্কার করা।

রিফ্যাক্টরিং বাস্তবায়ন করার সময় প্রক্রিয়ার সময় কোনো কার্যকারিতা ভঙ্গ না হওয়ার নিশ্চয়তা দেওয়ার জন্য একটি বিস্তৃত পরীক্ষা সেট থাকা জরুরি। C# এর জন্য ReSharper এর মতো স্বয়ংক্রিয় রিফ্যাক্টরিং টুলগুলি কোড কাঠামো পরিবর্তনের নিরাপদ উপায় প্রদান করে এই প্রক্রিয়াতে সাহায্য করতে পারে। তবে, টুলিং কোডবেস এবং কোডিং নীতিগুলির গভীর উপলব্ধির পরিপূরক হওয়া উচিত।

## আরও দেখুন
- রিফ্যাক্টরিং নিয়ে মার্টিন ফাউলারের মৌলিক কাজ: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- ভিজ্যুয়াল স্টুডিওতে রিফ্যাক্টরিং নিয়ে মাইক্রোসফটের গাইড: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- উদাহরণ সহ রিফ্যাক্টরিং প্যাটার্নগুলির বিস্তারিত তদন্ত: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
