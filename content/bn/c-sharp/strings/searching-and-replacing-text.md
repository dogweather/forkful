---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:15:40.699036-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
স্ট্রিংয়ে টেক্সট অনুসন্ধান এবং প্রতিস্থাপন আপনাকে ম্যানুয়াল সম্পাদনা ছাড়াই ডেটা আপডেট করতে দেয়। ব্যবহারকারীর ইনপুট সংশোধন, ডেটা বিন্যাসন, বা ব্যাচ আপডেটগুলি কার্যকরভাবে পরিচালনা করার জন্য প্রোগ্রামারদের এটি প্রয়োজন।

## কিভাবে:
C# টেক্সট ম্যানিপুলেশন বেশ সরল করে তোলে। নিচে, `string.Replace` পদ্ধতিটি দেখুন যার মাধ্যমে শব্দ প্রতিস্থাপন করা হয়।

```C#
using System;

public class Program
{
    public static void Main()
    {
        string phrase = "Hello, World!";
        string updatedPhrase = phrase.Replace("World", "C#");
        
        Console.WriteLine(updatedPhrase); // আউটপুট: Hello, C#!
    }
}
```

রকেট বিজ্ঞান নয়, তাই না? কিন্তু ধরুন আমরা কেস উপেক্ষা করতে চাই বা কেবল পূর্ণ শব্দগুলি প্রতিস্থাপন করতে চাই? রেজেক্স এখানে সাহায্যে আসে:

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string phrase = "Apples grow on trees. apple pies are tasty.";
        string pattern = "\\bapple\\b"; // \b রেজেক্সে একটি শব্দ সীমানা
        string replacement = "Orange";
        
        string updatedPhrase = Regex.Replace(phrase, pattern, replacement, RegexOptions.IgnoreCase);

        Console.WriteLine(updatedPhrase); // আউটপুট: Oranges grow on trees. Orange pies are tasty.
    }
}
```

## গভীরে ডুব দিন
অতীতে, স্ট্রিংগুলিকে ম্যানিপুলেট করা ঝামেলার ছিল। C ছিল আমাদের একমাত্র সম্পদ, এবং এর মানে ছিল অক্ষরের অ্যারে এবং ম্যানুয়াল ইটারেশনের সাথে মোকাবিলা করা। C# আমাদের এক উপহার দিয়েছে: সহজ স্ট্রিং হ্যান্ডলিং।

যদি `string.Replace` বা `Regex.Replace` আপনার প্রয়োজন পূরণ করতে না পারে, আমাদের কিছু বিকল্প আছে। বিশাল টেক্সট বা জটিল প্যাটার্নের জন্য, একটি কাস্টম পার্সার লেখার কথা বিবেচনা করুন অথবা যেমন Antlr মতো লাইব্রেরি ব্যবহার করুন।

রেজেক্স প্যাটার্ন ম্যাচিংয়ের জন্য শক্তিশালী কিন্তু ধীর হতে পারে। যদি পারফরম্যান্স গুরুত্বপূর্ণ হয় এবং আপনি সূক্ষ্ম বিস্তারিত সম্পর্কে আগ্রহী হন, তাহলে `StringBuilder` এর সাথে তুলনা করে মাপ নিন বিশাল, ইটারেটিভ প্রতিস্থাপনের জন্য।

## দেখুন এছাড়াও
- Microsoft Docs [`string.Replace`](https://docs.microsoft.com/dotnet/api/system.string.replace) সম্পর্কে
- .NET-এর [`Regex`](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex) ক্লাস বিস্তারিত প্যাটার্নের জন্য
- জটিল পার্সিংয়ের জন্য Antlr চেক আউট করুন: [The ANTLR Mega Tutorial](https://tomassetti.me/antlr-mega-tutorial/)
