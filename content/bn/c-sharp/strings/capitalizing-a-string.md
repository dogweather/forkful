---
title:                "স্ট্রিং এর প্রথম অক্ষর বড় হাতের করা"
date:                  2024-03-17T17:46:10.906905-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
C# এ একটি স্ট্রিংকে বড় হাতের অক্ষরে পরিণত করা মানে হল স্ট্রিংটির প্রথম অক্ষরটি বড় হাতের অক্ষরে পরিণত করা যদি তা ইতিমধ্যে বড় হাতের না হয়। এই পরিবর্তনটি আউটপুটগুলি ফরম্যাটিং, কোডিং মানদণ্ড প্রয়োগ, অথবা ব্যবহারকারীর ইন্টারফেস টেক্সটগুলি আরও পাঠযোগ্য করার জন্য গুরুত্বপূর্ণ হতে পারে।

## কিভাবে:
C# স্ট্রিংগুলিকে বড় হাতে পরিণত করার জন্য বিল্ট-ইন পদ্ধতিগুলির মাধ্যমে একটি সহজাত পদ্ধতি প্রদান করে। এটি প্রাপ্ত করার সবচেয়ে সহজে পদ্ধতি হল সরাসরি এই পদ্ধতিগুলির মাধ্যমে স্ট্রিংটিকে পরিবর্তন করা। আরও জটিল বা বিশেষ বড় হাতের নিয়মগুলির জন্য (উদাঃ, প্রতি শব্দে বড় হাতের অক্ষরে পরিণত করা), অতিরিক্ত লাইব্রেরি বা ম্যানুয়াল পদ্ধতিগুলি প্রয়োজনীয় হতে পারে। নিচে C# এ বিভিন্ন উপায়ে একটি স্ট্রিংকে বড় হাতে কিভাবে পরিণত করা যায় তার উদাহরণ দেওয়া হল।

### মৌলিক বড় হাতের পরিবর্তন:
একটি শব্দ অথবা বাক্যের প্রথম অক্ষর বড় হাতে পরিণত করার জন্য:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // আউটপুট: "Hello world"
```

### প্রতিটি শব্দ বড় হাতে:
একটি স্ট্রিংএর প্রতিটি শব্দের প্রথম অক্ষর বড় হাতে পরিণত করার জন্য, আপনি `System.Globalization` নেমস্পেসে পাওয়া `TextInfo.ToTitleCase` পদ্ধতিটি ব্যবহার করতে পারেন:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // আউটপুট: "Hello World"
```

লক্ষ্য করুন: `ToTitleCase` অক্ষরগুলির অবশিষ্টাংশকে ছোট হাতে পরিণত করে না; এটি শুধু প্রতিটি শব্দের প্রথম অক্ষরকে বড় হাতে পরিণত করে। তাছাড়া, কিছু কিছু শব্দ (`and`, `or`, `of` এর মতো) টাইটেল কেস নিয়মগুলির ভিত্তিতে বড় হাতে নাও হতে পারে যা সাংস্কৃতিক সেটিংসের উপর নির্ভর করে।

### পুনঃব্যবহারযোগ্যতার জন্য এক্সটেনশন মেথড ব্যবহার:
আপনি একটি এক্সটেনশন মেথড `string` ক্লাসের জন্য তৈরি করে বড় হাতের প্রক্রিয়াটি সরল করতে এবং আপনার কোডকে পরিষ্কার এবং বেশি পুনঃব্যবহারযোগ্য করতে পারেন। এমন একটি মেথড তৈরি করা এবং ব্যবহার করা কিভাবে তা নিচে দেখানো হল:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // আউটপুট: "Hello world"
    }
}
```

এই এক্সটেনশন মেথড `Capitalize` টি নেমস্পেসের মধ্যে যেকোনো স্ট্রিং অবজেক্টে কল করা যেতে পারে, যা C# এ স্ট্রিং ম্যানিপুলেশনে আরও অন্তর্জ্ঞানমূলক এবং অবজেক্ট-ওরিয়েন্টেড পদ্ধতি প্রদান করে।

### থার্ড-পার্টি লাইব্রেরিগুলি:
যদিও C# এর স্ট্যান্ডার্ড লাইব্রেরি স্ট্রিংগুলিকে বড় হাতে পরিণত করার বেশিরভাগ প্রয়োজন পূরণ করে, কিছু বিশেষীকৃত কাজের জন্য থার্ড-পার্টি লাইব্রেরিগুলি, যেমন Humanizer, উপকারী হতে পারে। তবে, কেবল স্ট্রিংগুলিকে বা একটি স্ট্রিং এর প্রতিটি শব্দকে বড় হাতে পরিণত করার কাজের জন্য, স্ট্যান্ডার্ড C# পদ্ধতিগুলি যথেষ্ট এবং কার্যকর, বহিরাগত নির্ভরতা প্রয়োজন হয় না।
