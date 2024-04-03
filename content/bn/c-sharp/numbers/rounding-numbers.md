---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:14:07.762745-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0996\u09BE\u09A8\u09C7\
  \ C#-\u098F \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0997\u09CB\u09B2 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u09C7 \u098F\u0995\u099F\u09BF \u09B0\u09BE\
  \u0989\u09A8\u09CD\u09A1-\u099F\u09CD\u09B0\u09BF\u09AA \u099F\u09BF\u0995\u09BF\
  \u099F \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
lastmod: '2024-03-17T18:47:44.032151-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0996\u09BE\u09A8\u09C7 C#-\u098F \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0997\u09CB\u09B2 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B0\u09BE\u0989\u09A8\u09CD\u09A1-\u099F\u09CD\u09B0\u09BF\
  \u09AA \u099F\u09BF\u0995\u09BF\u099F \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09A8\u09BF\u09B0\u09CD\u09A3\u09DF"
weight: 13
---

## কিভাবে:
এখানে C#-এ সংখ্যা গোল করার জন্যে একটি রাউন্ড-ট্রিপ টিকিট রয়েছে:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // নিকটতম পূর্ণ সংখ্যায় গোল করুন
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // আউটপুট: 123

        // দশমিকের সংখ্যা নির্দিষ্ট করুন
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // আউটপুট: 123.46

        // পরবর্তী অঙ্ক যেমনই হোক না কেন, উর্ধ্বে গোল করুন  
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // আউটপুট: 124

        // পরবর্তী অঙ্ক যেমনই হোক না কেন, নীচে গোল করুন
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // আউটপুট: 123
    }
}
```

## গভীরে ডুব:
অতীতে, গণনা খরচ ছাঁটাইয়ের জন্য সংখ্যা গোল করা একটি চিন্তাভাবনাহীন কাজ ছিল। প্রতিটি চক্র গণনা করা হয়েছিল, এবং সংখ্যা ছাঁটাই মূল্যবান সময় বাঁচিয়েছিল। আধুনিক C#-এ এগিয়ে আসতে, এটি ডাবলস এবং দশমিকের নিখুঁততার প্রবণতা এবং প্রদর্শনের বিচ্ছিন্নতা সামাল দেওয়ার বিষয়ে হয়ে উঠেছে।

`Math.Round`, `Math.Floor`, এবং `Math.Ceiling` ছাড়াও, `MidpointRounding` enum আমাদের মধ্যম অবস্থানে আটকা পড়া সংখ্যাগুলির ভাগ্য নির্ধারণ করতে দেয়—এটি ব্যাঙ্কিং নিয়ম এবং "রাউন্ড হাফ আপ" এর খেলার মাঠের ন্যায়বিচারের মাঝামাঝি।

গম্ভীর সমস্যার মুখে, যেমন গাণিতিক বা আর্থিক অ্যাপ্লিকেশন, আমাদের কাছে `double` এর চেয়ে `decimal` রয়েছে, যা উচ্চ নির্ভুলতা প্রদান করে—কম গোল করা, কম সমস্যা।

## দেখুন এছাড়াও
- [অফিসিয়াল C# ডক্‌সে `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: আমি কখন Decimal এর পরিবর্তে Double ব্যবহার করবো?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [ফ্লোটিং-পয়েন্ট অ্যারিথমেটিকের জন্য IEEE স্ট্যান্ডার্ড (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
