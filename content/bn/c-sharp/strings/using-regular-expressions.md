---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:38.174699-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: #."
lastmod: '2024-03-17T18:47:44.027248-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:


### সহজ প্যাটার্ন মিলান
একটি স্ট্রিং নির্দিষ্ট প্যাটার্ন ধারণ করে কিনা তা যাচাই করতে, আপনি `System.Text.RegularExpressions` নেমস্পেস থেকে `Regex.IsMatch` মেথড ব্যবহার করতে পারেন।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // আউটপুট: True
    }
}
```

### ডেটা এক্সট্রাক্ট করা
একটি স্ট্রিং থেকে রেগেক্সে গ্রুপের মাধ্যমে ডেটা এক্সট্রাক্ট করা হয় `Regex.Match` মেথডের মাধ্যমে।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // আউটপুট: Year: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // আউটপুট: Month: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // আউটপুট: Day: 12
        }
    }
}
```

### টেক্সট প্রতিস্থাপন
`Regex.Replace` মেথডের মাধ্যমে একটি স্ট্রিংয়ের নির্দিষ্ট প্যাটার্ন মিলানের ভিত্তিতে টেক্সট প্রতিস্থাপন করা সম্ভব।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // আউটপুট: Visit Google!
    }
}
```

### স্ট্রিং বিভাজন
`Regex.Split` মেথডের মাধ্যমে নির্দিষ্ট রেগেক্স প্যাটার্নের ভিত্তিতে একটি স্ট্রিংকে অ্যারেতে বিভাজন করা যায়।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // আউটপুট:
        // one
        // two
        // three
        // four
        // five
    }
}
```

### তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার
নিয়মিত প্রকাশনাগুলির জন্য .NET ফ্রেমওয়ার্ক ব্যাপক সাপোর্ট প্রদান করলেও, `PCRE.NET` এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলি পার্ল-সামঞ্জস্যপূর্ণ নিয়মিত প্রকাশনা (PCRE) C#-এ অফার করে। যদি আপনার .NET-এর বাস্তবায়নে পার্লের রেগেক্স ইঞ্জিন থেকে উপলব্ধ নয় এমন ফিচার বা সিনট্যাক্সের প্রয়োজন হয় তবে এটি উপকারী হতে পারে।

`PCRE.NET` ব্যবহার করতে, আপনি প্রথমে এর NuGet প্যাকেজ ইনস্টল করবেন, এবং তারপর আপনি এটি নেটিভ .NET regex ক্লাসগুলির মতো ব্যবহার করতে পারেন।

```csharp
// PCRE.NET ব্যবহার করে উদাহরণ এখানে
// নোট: PCRE.NET-এর একটি অনন্য ফিচার প্রদর্শন করতে উপরেরগুলোর মতো একটি নমুনা কল্পনা করুন।
```

নিয়মিত প্রকাশনাগুলির জন্য তৃতীয়-পক্ষের লাইব্রেরিগুলি ইন্টিগ্রেট করার সময়, সবসময় তাদের দলিলিকরণে বিস্তারিত ব্যবহার এবং সামঞ্জস্যতা তথ্যের জন্য পরামর্শ নিন।
