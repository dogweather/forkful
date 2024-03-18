---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:27.264497-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09B9\u09B2 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\
  \u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09AF\u09BE\u09A4\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09A8\
  \u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09B2\u09CB\u09A1 \u0995\u09B0\
  \u09BE, \u09A1\u09C7\u099F\u09BE \u09AA\u09A1\u09BC\u09BE \u0985\u09A5\u09AC\u09BE\
  \ \u09B8\u09C7\u0987\u09B8\u09AC \u09B8\u09AE\u09CD\u09AA\u09A6 \u0986\u09B9\u09B0\
  \u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.055723-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE \u09B9\u09B2 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u09AB\u09BE\
  \u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09AF\u09BE\u09A4\u09C7 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09A8\u09AB\
  \u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09B2\u09CB\u09A1 \u0995\u09B0\u09BE\
  , \u09A1\u09C7\u099F\u09BE \u09AA\u09A1\u09BC\u09BE \u0985\u09A5\u09AC\u09BE \u09B8\
  \u09C7\u0987\u09B8\u09AC \u09B8\u09AE\u09CD\u09AA\u09A6 \u0986\u09B9\u09B0\u09A3\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7\u2026"
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
টেক্সট ফাইল পড়া হল এমন একটি ফাইল থেকে ডেটা নেওয়া যাতে টেক্সট রয়েছে। প্রোগ্রামাররা এটি কনফিগারেশন লোড করা, ডেটা পড়া অথবা সেইসব সম্পদ আহরণ করার জন্য করে থাকে যা হার্ড-কোড করা বা অনুপযুক্ত।

## কিভাবে:
চলুন সরাসরি বিষয়ে যাই। এখানে আপনি কিভাবে C#`System.IO` ব্যবহার করে একটি ফাইল থেকে পড়বেন তা দেখানো হল।

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\path\to\your\file.txt";
        
        // সমস্ত টেক্সট পড়া
        string allText = File.ReadAllText(filePath);
        Console.WriteLine(allText);
        
        // লাইনগুলি একটি অ্যারেতে পড়া
        string[] lines = File.ReadAllLines(filePath);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
        
        // StreamReader এর সাহায্যে পড়া
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

নমুনা আউটপুট:

```
হ্যালো, এটি একটি টেক্সট ফাইল।
এতে একাধিক লাইন রয়েছে।
প্রতিটি লাইন আলাদা আলাদা ভাবে পড়া হবে।
```

## গভীর ডুব
টেক্সট ফাইল পড়া যথেষ্ট সহজ মনে হয়, তাই না? কিন্তু এর পেছনে কিছু ইতিহাস এবং জানার মতো কিছু বিষয় রয়েছে।

অতীতে, টেক্সট ফাইলগুলি প্রায়ই ডেটা সঞ্চয় করার প্রধান উপায় ছিল যখন ডাটাবেসগুলি প্রচলিত ছিল না। প্রোগ্রামারদের ফাইল অ্যাক্সেস ব্যবস্থাপনা করতে হতো, ডেটা সঠিক ফর্ম্যাট করতে হতো এবং ত্রুটিগুলি সামলাতে হতো। সেই থেকে C# অনেক উন্নত হয়েছে। এখন, `System.IO` আপনার ফাইল অপারেশনের জন্য প্রধান নেইমস্পেস।

আপনার অপশনগুলি রয়েছে:

- `File.ReadAllText` এক যাত্রায় সমগ্রটা পড়ে—ছোট ফাইলের জন্য দারুণ।
- `File.ReadAllLines` প্রতিটি লাইনকে একটি অ্যারে উপাদান হিসেবে দেয়—লাইন প্রসেস করা সুবিধাজনক।
- `StreamReader` লাইন দ্বারা লাইন পড়ে, যা বৃহৎ ফাইলের জন্য আরও মেমরি দক্ষ।

প্রতিটি পদ্ধতি ব্যবহারের সময় ফাইলটিকে লক করে। অন্যান্য প্রক্রিয়া যদি ফাইলে অ্যাক্সেস করতে চায় তবে এটি গুরুত্বপূর্ণ।

মনে রাখবেন, ফাইল নিয়ে কাজ করার সময় `FileNotFoundException` বা `IOException` এর মতো ব্যতিক্রমগুলি সামলানো অত্যন্ত জরুরি। আপনি আপনার অ্যাপ্লিকেশনকে অপ্রত্যাশিতভাবে ক্র্যাশ করতে চাইবেন না।

## আরও দেখুন
আরও প্রশ্ন আছে বা আপনার জ্ঞান প্রসারিত করতে চান? এই লিংকগুলি দেখুন:

- [MSDN ডকুমেন্টেশন ফাইল ক্লাসের উপর](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [MSDN ডকুমেন্টেশন StreamReader ক্লাসের উপর](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [ব্যতিক্রম সামলানো উপর টিউটোরিয়াল](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)
