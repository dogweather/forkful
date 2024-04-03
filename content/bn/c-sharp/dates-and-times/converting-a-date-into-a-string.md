---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:20.934953-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C#-\u098F, \u0986\u09AA\u09A8\u09BF\
  \ `DateTime` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u098F\u09AC\u0982 \u098F\
  \u099F\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982-\u098F \u09AA\
  \u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\u09B0 \u09AC\u09B9\u09C1 \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09AA\u09BE\u09AC\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\
  \u09C7 \u0995\u09AF\u09BC\u09C7\u0995\u099F\u09BF \u0989\u09AA\u09BE\u09AF\u09BC\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983."
lastmod: '2024-03-17T18:47:44.049565-06:00'
model: gpt-4-0125-preview
summary: "C#-\u098F, \u0986\u09AA\u09A8\u09BF `DateTime` \u0985\u09AC\u099C\u09C7\u0995\
  \u09CD\u099F \u098F\u09AC\u0982 \u098F\u099F\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982-\u098F \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\u09BE\
  \u09B0 \u09AC\u09B9\u09C1 \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09BE\u09AC\u09C7\
  \u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09AF\u09BC\u09C7\u0995\u099F\
  \u09BF \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\
  \u09B2\u0983."
title: "\u09A4\u09BE\u09B0\u09BF\u0996\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u098F \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE"
weight: 28
---

## কিভাবে:
C#-এ, আপনি `DateTime` অবজেক্ট এবং এটিকে স্ট্রিং-এ পরিণত করার বহু উপায় পাবেন। এখানে কয়েকটি উপায় দেওয়া হলঃ

```csharp
DateTime now = DateTime.Now;
string defaultString = now.ToString(); // ডিফল্ট ফরম্যাট
string specificFormat = now.ToString("yyyy-MM-dd"); // মানসম্মত ফরম্যাট, এখানে ISO 8601
string withCulture = now.ToString("d", new CultureInfo("en-US")); // মার্কিন সংস্কৃতির ছোট তারিখ

Console.WriteLine(defaultString); // আউটপুট সিস্টেমের সংস্কৃতি সেটিংস অনুযায়ী
Console.WriteLine(specificFormat); // আউটপুট: "2023-04-01"
Console.WriteLine(withCulture); // আউটপুট: "4/1/2023"
```

## গভীর বিশ্লেষণ
অনেক আগে, তারিখ ও স্ট্রিং ম্যানিপুলেশন আরও জটিল ছিল। আজকাল, C#-এর `DateTime` কালচার এবং ফর্ম্যাট সংক্রান্ত ওভারলোড সহ `.ToString()` প্রদান করে। `IFormatProvider` ইন্টারফেস, যেমন `CultureInfo`, কালচার-নির্দিষ্ট ফর্ম্যাটিং নিয়ন্ত্রণ করে।

বিকল্প? অবশ্যই! `String.Format` এবং ইন্টারপোলেশন (`$"{now:yyyy-MM-dd}"`) কনটেক্সটে তারিখগুলি স্ট্রিং-এ ইন্সার্ট করার জন্য বিকল্প। `DateTimeOffset` সময় অঞ্চল নির্দিষ্ট কাজের জন্য হাতিয়ার।

বাস্তবায়নের দিক থেকে, `DateTime` একটি স্ট্রাক্ট, অতএব একটি মানের ধরণ। এটিকে রূপান্তর করলে মূল পরিবর্তিত হয় না: অপরিবর্তনশীলতার জন্য জয়ী। আপনার শ্রোতা (ব্যবহারকারীরা) এবং আপনি যে সিস্টেমের সাথে ইন্টারফেস করছেন (ডাটাবেস, API) তার ভিত্তিতে আপনার স্ট্রিং ফর্ম্যাট বুদ্ধিমানভাবে নির্বাচন করুন।

## আরও দেখুন
- [DateTime.ToString Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [কাস্টম তারিখ এবং সময় ফর্ম্যাট স্ট্রিং](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
