---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:52.817201-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2\u09CB \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F\u09C7\u09B0 \u09B8\u09AE\u09B8\u09CD\u09A4 \u09AC\u09DC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0\
  \ \u09B8\u09AE\u09A4\u09C1\u09B2\u09CD\u09AF\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09BF\u09B6\u09C7\u09B7 \u0995\
  \u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.024261-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\
  \u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\
  \u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2\u09CB \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F\u09C7\u09B0 \u09B8\u09AE\u09B8\u09CD\u09A4 \u09AC\u09DC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0\
  \ \u09B8\u09AE\u09A4\u09C1\u09B2\u09CD\u09AF\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4\
  \ \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09BF\u09B6\u09C7\u09B7 \u0995\
  \u09B0\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিংকে ছোট হাতের অক্ষরে রূপান্তর করার অর্থ হলো টেক্সটের সমস্ত বড় হাতের অক্ষরগুলিকে তাদের ছোট হাতের সমতুল্যে পরিণত করা। প্রোগ্রামাররা এটি বিশেষ করে কেস-অসংবেদী তুলনা, অনুসন্ধান বা প্রসেসিংয়ের আগে ডাটা ইনপুটগুলি মানকরণ করার জন্য করে থাকেন।

## কিভাবে:
C# এ, আপনি `ToLower()` বা `ToLowerInvariant()` মেথড ব্যবহার করে একটি স্ট্রিংকে ছোট হাতের অক্ষরে রূপান্তর করতে পারেন। এই রকম:

```C#
string originalText = "Hello, World!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText); // মুদ্রণ করে: hello, world!
```

এবং সাংস্কৃতিক-অসংবেদী রূপান্তরের জন্য:

```C#
string mixedCaseText = "İstanbul";
string lowerInvariantText = mixedCaseText.ToLowerInvariant();

Console.WriteLine(lowerInvariantText); // মুদ্রণ করে: i̇stanbul
```

নমুনা আউটপুট:

```
hello, world!
i̇stanbul
```

## গভীর অনুসন্ধান
ঐতিহাসিকভাবে, স্ট্রিংগুলিকে ছোট হাতের অক্ষরে রূপান্তর করার প্রয়োজন সেই কম্পিউটার সিস্টেমগুলিতে উদ্ভূত হয়, যা কেস-অসংবেদী কমান্ড দিয়ে শুরু করে। বর্তমানে, আমরা এটি মূলত তিনটি মুখ্য কারণে করে থাকি:

1. **সমতা**: ইনপুটগুলি, বিশেষ করে ব্যবহারকারী-উৎপন্ন ডাটা চিকিৎসা করার সময়, ছোট হাতের অক্ষরে রূপান্তর করা একটি মানকৃত ফর্ম্যাট নিশ্চিত করে।
2. **কেস-অসংবেদী অপারেশন**: এটি অন্তর্ভুক্ত সার্চিং, সোর্টিং, এবং স্ট্রিংগুলি তুলনা করা যেখানে "Apple" এবং "apple" একইভাবে বিবেচিত হওয়া উচিত।
3. **লোকালাইজেশন**: ভাষাগুলিতে কেসিং নিয়ে ভিন্ন নিয়ম রয়েছে। `ToLowerInvariant()` এটি একটি সাংস্কৃতিক-অসংবেদী রূপান্তর সরবরাহ করে, অন্য ভাষাগুলির (ইংরেজির মতো) ভিত্তিতে অক্ষরগুলিকে ছোট হাতের অক্ষরে রূপান্তর করে এবং অনাকাঙ্ক্ষিত ফলাফলগুলি এড়ায়।

`.ToLower()` এবং `.ToLowerInvariant()` এর বিক্লপ হিসেবে নিয়মিত অভিব্যক্তি ব্যবহার করা বা ব্যক্তিগত রূপান্তর পরিস্থিতিতে একটি স্ট্রিং ম্যানুয়ালি ইটারেট করা অন্তর্ভুক্ত।

বাস্তবায়নের বিস্তারিত দিক দিয়ে, এই পদ্ধতিগুলি মূল স্ট্রিংটি পরিবর্তন করে না; .NET এ স্ট্রিংগুলি অপরিবর্তনীয়। তারা মূল স্ট্রিংএর ছোট হাতের সংস্করণ হিসেবে একটি নতুন স্ট্রিং তৈরি এবং ফিরিয়ে দেয়।

## আরও দেখুন
- C# ডকুমেন্টেশনে স্ট্রিং ক্লাস: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string)
- StringComparison Enum এবং সাংস্কৃতিক-অসংবেদী তুলনা: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)
