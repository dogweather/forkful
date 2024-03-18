---
title:                "তারিখকে স্ট্রিং এ রূপান্তর করা"
date:                  2024-03-17T17:46:20.934953-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

C#-এ একটি তারিখকে স্ট্রিং-এ রূপান্তর করা মূলত DateTime অবজেক্ট থেকে টেক্সট প্রতিনিধিত্বে বিন্যাস পরিবর্তন করার প্রক্রিয়া। প্রোগ্রামাররা এটি তারিখগুলি ব্যবহারকারী বান্ধব ফর্ম্যাটে প্রদর্শনের জন্য অথবা ডেটা স্টোরেজ এবং সংক্রমণের জন্য সিরিয়ালাইজ করার জন্য করে থাকেন।

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
