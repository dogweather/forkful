---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:56.421112-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: **\u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982:** \u098F\u0995\u099F\u09BF \u09B8\u09CD\
  \u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 `DateTime`-\u098F \u09B0\
  \u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ `DateTime.Parse` \u098F\u09AC\u0982 `DateTime.TryParse` \u09AE\u09C7\u09A5\u09A1\
  \u0997\u09C1\u09B2\u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\u09AE\u09BF\u0995 \u09AA\
  \u099B\u09A8\u09CD\u09A6\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:44.047296-06:00'
model: gpt-4-0125-preview
summary: "**\u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982\
  :**\n\n\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\
  \u09C7\u0995\u09C7 `DateTime`-\u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `DateTime.Parse` \u098F\u09AC\u0982 `DateTime.TryParse`\
  \ \u09AE\u09C7\u09A5\u09A1\u0997\u09C1\u09B2\u09BF \u09AA\u09CD\u09B0\u09BE\u09A5\
  \u09AE\u09BF\u0995 \u09AA\u099B\u09A8\u09CD\u09A6\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\
  \u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
**বেসিক পার্সিং:**

একটি স্ট্রিং থেকে `DateTime`-এ রূপান্তরের জন্য `DateTime.Parse` এবং `DateTime.TryParse` মেথডগুলি প্রাথমিক পছন্দ। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"সফলভাবে পার্স করা হয়েছে: {parsedDate}");
}
else
{
    Console.WriteLine("পার্স করতে ব্যর্থ হয়েছে.");
}
// আউটপুট: সফলভাবে পার্স করা হয়েছে: 4/12/2023 12:00:00 AM
```

**একটি সাংস্কৃতিক ফরম্যাট নির্দিষ্ট করে:**

কখনও কখনও, আপনাকে এমন একটি সাংস্কৃতিক ফর্ম্যাটে তারিখ স্ট্রিং পার্স করতে হতে পারে। এটি করতে আপনি `CultureInfo` ক্লাস ব্যবহার করতে পারেন:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// আউটপুট: 4/12/2023 12:00:00 AM
```

**নির্দিষ্ট ফরম্যাটের সাথে একটি পার্সিং:**

যে দৃশ্যগুলোতে তারিখগুলি একটি নির্দিষ্ট ফরম্যাটে আসে যা স্ট্যান্ডার্ড নাও হতে পারে, `DateTime.ParseExact` উপযোগী হয়ে ওঠে:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime(parsedDate) = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// আউটপুট: 4/12/2023 12:00:00 AM
```

**NodaTime ব্যবহার করে:**

আরও মজবুত তারিখ এবং সময় পার্সিংয়ের জন্য, জনপ্রিয় তৃতীয়-পক্ষের লাইব্রেরি NodaTime ব্যবহার করুন। এটি .NET অ্যাপ্লিকেশনে জটিল তারিখ এবং সময় হ্যান্ডলিংয়ের জন্য সময় জোন, পিরিয়ড এবং দৈর্ঘ্য ধারণা, এবং অনেক ভিন্ন ক্যালেন্ডার সিস্টেমের ব্যাপক সাপোর্ট অফার করে, যা এটিকে একটি শক্তিশালী বিকল্প করে তোলে।

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("পার্স করতে ব্যর্থ হয়েছে.");
}
```
