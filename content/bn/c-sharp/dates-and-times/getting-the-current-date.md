---
title:                "বর্তমান তারিখ পেতে"
date:                  2024-03-17T17:48:47.768224-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
C#-এ বর্তমান তারিখ পাওয়া মানে সিস্টেম থেকে বর্তমান তারিখ এবং সময়ের বিবরণ আনা। প্রোগ্রামাররা প্রায়ই লগিং, টাইমস্ট্যাম্পিং অপারেশন অথবা অ্যাপ্লিকেশনের মধ্যে কাজগুলি সময়মতো করে দেওয়ার জন্য এই তথ্যের প্রয়োজন পড়ে, যাতে কর্মগুলি সঠিক সময়ে সম্পন্ন হয় এবং ডেটা নির্দিষ্ট টাইমস্ট্যাম্পের সাথে চিহ্নিত করা হয়।

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
