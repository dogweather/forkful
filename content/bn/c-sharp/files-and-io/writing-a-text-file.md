---
title:                "একটি টেক্সট ফাইল লিখা"
date:                  2024-03-17T18:39:35.194942-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
C# ভাষায় টেক্সট ফাইল লিখন হলো প্রোগ্রামের মাধ্যমে ফাইল সিস্টেমে টেক্সট ফাইল তৈরি করা বা পরিবর্তন করা - যা অনেক অ্যাপ্লিকেশনের জন্য একটি মৌলিক কার্য, যেমন লগিং, ডেটা এক্সপোর্টিং, অথবা কন্ফিগারেশন ম্যানেজমেন্ট। প্রোগ্রামাররা এই অপারেশন সম্পাদন করেন সেশনের মধ্যে ডেটা সংরক্ষণ, সিস্টেম জুড়ে তথ্য শেয়ার করা, বা মানব-পাঠ্য আউটপুট সংরক্ষণ করার জন্য।

## কিভাবে:
C# তার `System.IO` নেমস্পেসের মাধ্যমে ফাইল অপারেশন সহজ করে তোলে, টেক্সট ফাইল লেখার সরল পদ্ধতি সরবরাহ করে। এখানে একটি বেসিক টেক্সট ফাইল লেখার এবং একটি বিদ্যমান ফাইলে টেক্সট যোগ করার উপায় দেওয়া হলো।

### শুরু থেকে একটি টেক্সট ফাইলে লেখা
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hello, world!";

        // নতুন ফাইলে কন্টেন্ট লেখা
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("ফাইল সফলভাবে লেখা হয়েছে।");
    }
}
```
**স্যাম্পল আউটপুট:**
```
ফাইল সফলভাবে লেখা হয়েছে।
```

### একটি বিদ্যমান ফাইলে টেক্সট যোগ করা
আপনি যদি একটি বিদ্যমান ফাইলের শেষে টেক্সট যোগ করতে চান, তাহলে `File.AppendAllText` মেথড ব্যবহার করতে পারেন।

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nAdding more content.";

        // ফাইলে কন্টেন্ট যোগ করা
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("কন্টেন্ট সফলভাবে যোগ করা হয়েছে।");
    }
}
```
**স্যাম্পল আউটপুট:**
```
কন্টেন্ট সফলভাবে যোগ করা হয়েছে।
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার: `StreamWriter`
লেখার উপর আরও সূক্ষ্ম নিয়ন্ত্রণ, স্বয়ংক্রিয় ফ্লাশিং এবং এনকোডিং নির্বাচনের জন্য `StreamWriter` ব্যবহার করুন।

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "This is an example using StreamWriter.";

        // StreamWriter ব্যবহার করে ফাইলে লেখা
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("StreamWriter দ্বারা ফাইল সফলভাবে লেখা হয়েছে।");
    }
}
```
**স্যাম্পল আউটপুট:**
```
StreamWriter দ্বারা ফাইল সফলভাবে লেখা হয়েছে।
```

এই পদ্ধতিগুলো ভিন্ন ভিন্ন প্রয়োজন পরিবেশন করে: দ্রুত অপারেশনের জন্য সরাসরি `File` মেথডগুলি, এবং আরও জটিল লেখা পরিস্থিতিগুলির জন্য `StreamWriter`। আপনার নির্দিষ্ট প্রয়োজন অনুযায়ী নির্বাচন করুন, পারফরমেন্স এবং ফাইলের আকারের মতো ফ্যাক্টরগুলি বিবেচনা করে।
