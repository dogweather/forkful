---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:47.768224-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: C# `.NET Framework`-\u098F\u09B0\
  \ System \u09A8\u09BE\u09AE\u09B8\u09CD\u09A5\u09BE\u09A8\u09C7\u09B0 \u0985\u0982\
  \u09B6 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09AC\u09B0\u09CD\
  \u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\
  \u09BC\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09A8\
  \u09C0\u099A\u09C7\u09B0 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.048581-06:00'
model: gpt-4-0125-preview
summary: "C# `.NET Framework`-\u098F\u09B0 System \u09A8\u09BE\u09AE\u09B8\u09CD\u09A5\
  \u09BE\u09A8\u09C7\u09B0 \u0985\u0982\u09B6 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7\
  \ `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\
  \u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0 \u098F\u0995\u099F\
  \u09BF \u09B8\u09B0\u09B2 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09A8\u09C0\u099A\u09C7\u09B0 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3\u099F\u09BF \u09A6\u09C7\u0996\u09BE\u09AF\u09BC \u0995\
  \u09C0\u09AD\u09BE\u09AC\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09AF\
  \u09BC, \u098F\u09AC\u0982 \u0990\u099A\u09CD\u099B\u09BF\u0995\u09AD\u09BE\u09AC\
  \u09C7, \u09B8\u09AE\u09AF\u09BC\u0993\u0964."
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
weight: 29
---

## কীভাবে:
C# `.NET Framework`-এর System নামস্থানের অংশ হিসেবে `DateTime` ক্লাস ব্যবহার করে বর্তমান তারিখ পাওয়ার একটি সরল উপায় প্রদান করে। নীচের উদাহরণটি দেখায় কীভাবে বর্তমান তারিখ পাওয়া যায়, এবং ঐচ্ছিকভাবে, সময়ও।

```csharp
using System;

class Program
{
    static void Main()
    {
        // শুধুমাত্র বর্তমান তারিখ পায়
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // আউটপুট: MM/dd/yyyy
        
        // বর্তমান তারিখ এবং সময় পায়
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // আউটপুট: MM/dd/yyyy HH:mm:ss

        // বর্তমান UTC তারিখ এবং সময় পায়
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // আউটপুট: MM/dd/yyyy HH:mm:ss
    }
}
```

তৃতীয়-পক্ষের লাইব্রেরি হিসাবে, NodaTime বিভিন্ন ক্যালেন্ডার এবং সময় অঞ্চলে বর্তমান তারিখ আনার জন্য, সময় এবং তারিখ ম্যানিপুলেশনের একটি শক্তিশালী বিকল্প প্রদান করে যা ব্যবহার করা হয়েছে।

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // ISO ক্যালেন্ডারে বর্তমান তারিখ পেতে NodaTime ব্যবহার করা হচ্ছে
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // আউটপুট: yyyy-MM-dd

        // টাইমজোন-স্পেসিফিক তারিখ গুলোর জন্য
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // আউটপুট: yyyy-MM-dd
    }
}
```

এটি `DateTime` ক্লাসের সাথে প্রাথমিক ব্যবহার এবং NodaTime দ্বারা প্রদত্ত উন্নত ক্ষমতাগুলি দেখায়, বিশেষ করে বিভিন্ন সময় অঞ্চল অথবা ক্যালেন্ডার সিস্টেম মোকাবিলা
করার জন্য প্রয়োজনীয় অ্যাপ্লিকেশনগুলির জন্য উপকারী।
