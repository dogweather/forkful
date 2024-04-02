---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:42.822378-06:00
description: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0986\
  \u09AE\u09BE\u09A6\u09C7\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AC\u09CD\
  \u09AF\u09AC\u09B8\u09CD\u09A5\u09BE\u0995\u09C7 \u0995\u09B2\u09CD\u09AA\u09BF\u09A4\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09AC\
  \u09BF\u09B8\u09CD\u09A4\u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE\
  \ \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\u0995\u09C7 \u098F\u09AE\u09A8 \u09B8\u09AE\
  \u09C0\u0995\u09B0\u09A3 \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09A4\
  \u09C7 \u09B8\u09B9\u09BE\u09AF\u09BC\u09A4\u09BE \u0995\u09B0\u09C7 \u09AF\u09C7\
  \u0997\u09C1\u09B2\u09CB\u09B0 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u09B8\u09AE\
  \u09BE\u09A7\u09BE\u09A8 \u09A8\u09C7\u0987\u0964\u2026"
lastmod: '2024-03-17T18:47:44.031163-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0986\u09AE\
  \u09BE\u09A6\u09C7\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AC\u09CD\u09AF\
  \u09AC\u09B8\u09CD\u09A5\u09BE\u0995\u09C7 \u0995\u09B2\u09CD\u09AA\u09BF\u09A4\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09C7 \u09AC\
  \u09BF\u09B8\u09CD\u09A4\u09BE\u09B0\u09BF\u09A4 \u0995\u09B0\u09C7, \u09AF\u09BE\
  \ \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\u0995\u09C7 \u098F\u09AE\u09A8 \u09B8\u09AE\
  \u09C0\u0995\u09B0\u09A3 \u09B8\u09AE\u09BE\u09A7\u09BE\u09A8 \u0995\u09B0\u09A4\
  \u09C7 \u09B8\u09B9\u09BE\u09AF\u09BC\u09A4\u09BE \u0995\u09B0\u09C7 \u09AF\u09C7\
  \u0997\u09C1\u09B2\u09CB\u09B0 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u09B8\u09AE\
  \u09BE\u09A7\u09BE\u09A8 \u09A8\u09C7\u0987\u0964\u2026"
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কি এবং কেন?
জটিল সংখ্যা আমাদের সংখ্যা ব্যবস্থাকে কল্পিত সংখ্যা যোগ করে বিস্তারিত করে, যা আমাদেরকে এমন সমীকরণ সমাধান করতে সহায়তা করে যেগুলোর বাস্তব সমাধান নেই। প্রোগ্রামাররা প্রকৌশল, পদার্থবিদ্যা এবং সিগনাল প্রক্রিয়াকরণের মতো ক্ষেত্রগুলোতে এই সংখ্যাগুলির সাথে কাজ করেন যেখানে এই সংখ্যাগুলি মডেলিং এবং সমস্যা সমাধানের জন্য অপরিহার্য।

## কিভাবে:
C# এর মধ্যে `System.Numerics.Complex` কাঠামোটি জটিল সংখ্যাগুলি প্রক্রিয়া করার জন্য বিল্ট ইন আছে। এখানে একটি দ্রুত ভ্রমণ দেওয়া হলো:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // জটিল সংখ্যা তৈরি করা
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // বেসিক অপারেশনস
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // ফলাফল প্রদর্শন
        Console.WriteLine($"Sum: {sum}");
        Console.WriteLine($"Difference: {difference}");
        Console.WriteLine($"Product: {product}");
        Console.WriteLine($"Quotient: {quotient}");
        Console.WriteLine($"Magnitude of c1: {c1.Magnitude}");
        Console.WriteLine($"Phase of c1: {c1.Phase}");
    }
}
```

এবং তার ফলাফল হবে:

```
Sum: (4.70710678118655, 5.70710678118655)
Difference: (3.29289321881345, 4.29289321881345)
Product: (-1.00000000000001, 9)
Quotient: (0.6, 0.8)
Magnitude of c1: 6.40312423743285
Phase of c1: 0.896055384571344
```

## গভীর ডুব
জটিল সংখ্যা, যা একটি বাস্তব এবং একটি কল্পিত অংশ থেকে গঠিত (প্রায়শই a + bi হিসেবে নোট করা), 17শ শতাব্দী থেকে আছে। ইতালিয়ান গণিতজ্ঞ Gerolamo Cardano তাদের প্রাথমিক উন্নয়নের জন্য কৃতিত্ব পান। প্রোগ্রামিং তে, জটিল সংখ্যার সাথে কাজ করা মানে এই দুই পৃথক অংশ বোঝা এবং পরিচালনা করা।

C#'s `System.Numerics.Complex` ভাষার সাথে সমন্বিত এবং শক্তিশালী হলেও, Python এর মতো অন্য ভাষাগুলি `cmath` বা তৃতীয়-পক্ষের লাইব্রেরিগুলির সাথে অনুরূপ কার্যকারিতা অফার করে। এবং যদি আপনি পুরানো সংস্করণের C# বা .NET সংস্করণে কাজ করেন যেটি `System.Numerics` সমর্থন করে না, তবে আপনাকে নিজের জটিল সংখ্যা শ্রেণীটি তৈরি করতে হতে পারে অথবা একটি লাইব্রেরি খুঁজে বের করতে হতে পারে।

অভ্যন্তরীণভাবে, জটিল সংখ্যাগুলিতে অপারেশনগুলি ভাসমান-বিন্দু অঙ্ক ব্যবহার করে যা গোলমালের ত্রুটি প্রবেশ করতে পারে। সুতরাং, জটিল সংখ্যা প্রচুর ব্যবহার করে এমন অ্যালগরিদম বাস্তবায়ন করার সময়, এটি মনে রাখা এবং নির্ভুলতা এবং সঠিকতার প্রভাব বিবেচনা করা প্রধান।

## আরও দেখুন
1. `System.Numerics.Complex` এর C# রেফারেন্স: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. জটিল সংখ্যার গাণিতিক বিশ্লেষণে আরো গভীরে ডুব: https://mathworld.wolfram.com/ComplexNumber.html
3. বিকল্প বাস্তবায়ন এবং লাইব্রেরিগুলির জন্য, Math.NET Numerics দেখুন: https://numerics.mathdotnet.com/
