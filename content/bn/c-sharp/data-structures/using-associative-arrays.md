---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:25:03.143899-06:00
description: "\u0985\u09CD\u09AF\u09BE\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\
  \u09BF\u09AD \u0985\u09CD\u09AF\u09BE\u09B0\u09C7, \u0985\u09A5\u09AC\u09BE C# \u098F\
  \ \u09A1\u09BF\u0995\u09B6\u09A8\u09BE\u09B0\u09BF, \u0986\u09AA\u09A8\u09BE\u0995\
  \u09C7 \u099A\u09BE\u09AC\u09BF \u098F\u09AC\u0982 \u09AE\u09BE\u09A8\u09C7\u09B0\
  \ \u099C\u09CB\u09A1\u09BC\u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u098F\
  \u09AC\u0982 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\
  \u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u0996\u09C1\u09AC \u09A6\u09CD\u09B0\u09C1\
  \u09A4\u09AD\u09BE\u09AC\u09C7 \u0985\u09A8\u09A8\u09CD\u09AF \u09AA\u09B0\u09BF\
  \u099A\u09DF\u0995 \u0985\u09AC\u09B2\u09AE\u09CD\u09AC\u09A8\u09C7 \u09AE\u09BE\
  \u09A8 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.030183-06:00'
model: gpt-4-0125-preview
summary: "\u0985\u09CD\u09AF\u09BE\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\
  \u09BF\u09AD \u0985\u09CD\u09AF\u09BE\u09B0\u09C7, \u0985\u09A5\u09AC\u09BE C# \u098F\
  \ \u09A1\u09BF\u0995\u09B6\u09A8\u09BE\u09B0\u09BF, \u0986\u09AA\u09A8\u09BE\u0995\
  \u09C7 \u099A\u09BE\u09AC\u09BF \u098F\u09AC\u0982 \u09AE\u09BE\u09A8\u09C7\u09B0\
  \ \u099C\u09CB\u09A1\u09BC\u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3 \u098F\
  \u09AC\u0982 \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\
  \u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u0996\u09C1\u09AC \u09A6\u09CD\u09B0\u09C1\
  \u09A4\u09AD\u09BE\u09AC\u09C7 \u0985\u09A8\u09A8\u09CD\u09AF \u09AA\u09B0\u09BF\
  \u099A\u09DF\u0995 \u0985\u09AC\u09B2\u09AE\u09CD\u09AC\u09A8\u09C7 \u09AE\u09BE\
  \u09A8 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8\u2026"
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
weight: 15
---

## কি এবং কেন?
অ্যাসোসিয়েটিভ অ্যারে, অথবা C# এ ডিকশনারি, আপনাকে চাবি এবং মানের জোড়া সংরক্ষণ এবং পরিচালনা করতে দেয়। খুব দ্রুতভাবে অনন্য পরিচয়ক অবলম্বনে মান অনুসন্ধান করতে হলে তারা আপনার প্রথম পছন্দ, যা জটিল অ্যাপ্লিকেশনগুলোতে ডেটা পরিচালনাকে সহজ করে তোলে।

## কিভাবে:

C# এ, আপনি `Dictionary<TKey, TValue>` ক্লাস ব্যবহার করে অ্যাসোসিয়েটিভ অ্যারে নিয়ে কাজ করেন। নিচে শুরু করার জন্য একটি দ্রুত উদাহরণ দেওয়া হলো:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // একটি ডিকশনারি তৈরী করা
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // কী-মান জোড়া যোগ করা
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // এর কী ব্যবহার করে একটি মান অ্যাকসেস করা
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // একটি মান আপডেট করা
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // একটি কী-মান জোড়া মুছে ফেলা
        fruitBasket.Remove("Oranges");

        // ডিকশনারির ওপর পুনরাবৃত্তি করা
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
নমুনা আউটপুট:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

এই উদাহরণটি একটি ডিকশনারি তৈরি করা, যোগ, অ্যাক্সেস, আপডেট এবং মুছে ফেলার উপাদান এবং এর ওপর পুনরাবৃত্তি করার প্রদর্শনী।

## গভীর ডুব

অ্যাসোসিয়েটিভ অ্যারের ধারণাটি পার্ল এবং PHP এর মতো স্ক্রিপ্টিং ভাষাগুলোতে তারা ডেটার সংগ্রহ পরিচালনায় নমনীয়তা অফার করার ব্যবহারে ফিরে যায়। C# এ, `Dictionary<TKey, TValue>` হলো ডি ফ্যাক্টো বাস্তবায়ন, যা .NET Framework 2.0 এ চালু হয়েছিল। এটি ডেটা .NET Frameworkএকটি হ্যাশ টেবিলে সংরক্ষণ করে, যা দ্রুত অনুসন্ধান, যোগ এবং মুছে ফেলার ক্ষেত্রে দক্ষ করে তোলে।

তবে, ডিকশনারি যদিও অত্যন্ত বহুমুখী, সব সময় আপনার সেরা বাজি হতে পারে না। সাজানো সংগ্রহ বজায় রাখার জন্য, আপনি `SortedDictionary<TKey, TValue>` অথবা `SortedList<TKey, TValue>` এর দিকে তাকাতে পারেন, যা সেট করা এবং মুছে ফেলা অপারেশনের গতি কমে যাওয়ার বিনিময়ে সাজানো ক্রম অফার করে। মাল্টি থ্রেড নিরাপত্তার দাবির পরিস্থিতিতে, `ConcurrentDictionary<TKey, TValue>` অতিরিক্ত ব্যয় যোগ করে কিন্তু ম্যানুয়াল লকিং ছাড়াই একাধিক থ্রেড থেকে নিরাপদ অ্যাক্সেস নিশ্চিত করে।

শেষপর্যন্ত, C# এ একটি অ্যাসোসিয়েটিভ অ্যারের বাস্তবায়নের চয়ন আপনার বিশেষ প্রয়োজনের উপর নির্ভর করে, ক্রম, কর্মক্ষমতা, এবং থ্রেড নিরাপত্তা সম্পর্কিত।
