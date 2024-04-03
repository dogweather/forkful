---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:39:35.194942-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C# \u09A4\u09BE\u09B0 `System.IO`\
  \ \u09A8\u09C7\u09AE\u09B8\u09CD\u09AA\u09C7\u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7, \u099F\
  \u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE\
  \u09B0 \u09B8\u09B0\u09B2 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09B8\u09B0\u09AC\
  \u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\u0996\u09BE\u09B0 \u098F\u09AC\u0982\
  \u2026"
lastmod: '2024-03-17T18:47:44.056861-06:00'
model: gpt-4-0125-preview
summary: "C# \u09A4\u09BE\u09B0 `System.IO` \u09A8\u09C7\u09AE\u09B8\u09CD\u09AA\u09C7\
  \u09B8\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AB\u09BE\u0987\
  \u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u09B8\u09B9\u099C \u0995\u09B0\
  \u09C7 \u09A4\u09CB\u09B2\u09C7, \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09C7\u0996\u09BE\u09B0 \u09B8\u09B0\u09B2 \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\
  \u0995 \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09B2\u09C7\
  \u0996\u09BE\u09B0 \u098F\u09AC\u0982 \u098F\u0995\u099F\u09BF \u09AC\u09BF\u09A6\
  \u09CD\u09AF\u09AE\u09BE\u09A8 \u09AB\u09BE\u0987\u09B2\u09C7 \u099F\u09C7\u0995\
  \u09CD\u09B8\u099F \u09AF\u09CB\u0997 \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\
  \u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\u0964\n\n#."
title: "\u098F\u0995\u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\
  \u0987\u09B2 \u09B2\u09BF\u0996\u09BE"
weight: 24
---

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
