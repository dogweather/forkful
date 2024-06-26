---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:10.142560-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8\
  : \u0995\u09BF\u099B\u09C1 \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09BE\u09A6 \u09A6\
  \u09BF\u09A4\u09C7 \u099A\u09BE\u09A8? \u098F\u0996\u09BE\u09A8\u09C7 C# \u098F\
  \ \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\u09C7\u09A8."
lastmod: '2024-04-05T22:40:38.183269-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09BF\u099B\u09C1 \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09BE\u09A6\
  \ \u09A6\u09BF\u09A4\u09C7 \u099A\u09BE\u09A8?"
title: "\u098F\u0995\u099F\u09BF \u09A8\u09AE\u09C1\u09A8\u09BE \u09AE\u09C7\u09B2\
  \u09C7 \u0985\u0995\u09CD\u09B7\u09B0\u0997\u09C1\u09B2\u09BF \u09AE\u09C1\u099B\
  \u09C7 \u09AB\u09C7\u09B2\u09BE"
weight: 5
---

## কিভাবে করবেন:
কিছু অক্ষর বাদ দিতে চান? এখানে C# এ কিভাবে করবেন:

```C#
using System;
using System.Text.RegularExpressions;

class PatternDeletion
{
    static void Main()
    {
        string originalText = "B4n4n4 P1zza!";
        string pattern = @"[0-9]+"; // সব সংখ্যা অপসারণ করুন
        
        string cleanedText = Regex.Replace(originalText, pattern, string.Empty);
        
        Console.WriteLine(cleanedText); // আউটপুট: Bnnn Pzza!
    }
}
```
'a' এর পরে একটি সংখ্যা বাদ দিতে চান? দেখুন:

```C#
string targetedRemoval = "C4ndy C4ne";
string complexPattern = @"a[0-9]"; // 'a' এর পরে যে কোন সংখ্যা লক্ষ্য করে

string refinedText = Regex.Replace(targetedRemoval, complexPattern, string.Empty);

Console.WriteLine(refinedText); // আউটপুট: Cndy Cne
```

## গভীরে যান
রেজেক্স (রেগুলার এক্সপ্রেশনস) প্যাটার্ন-ম্যাচিং ক্ষমতাকে চালায়, যা ১৯৫০ এর দশকে তাত্ত্বিক মূলে ফিরে যায় (ধন্যবাদ, অটোমাটা তত্ত্ব!). রেজেক্সের বিকল্প হিসাবে সরল `String.Replace()` থাকে সাধারণ প্রতিস্থাপনের জন্য, অথবা যদি কর্মক্ষমতা জরুরি হয় তবে কাস্টম অ্যালগরিদম থাকে (কারণ রেজেক্সে কিছুটা ওভারহেড থাকে)। এই বিকল্পগুলি জটিল প্যাটার্নের জন্য রেজেক্সের যে নমনীয়তা এবং নির্ভুলতা আছে তা থাকে না। প্যাটার্ন অপসারণ বাস্তবায়ন করতে গিয়ে রেজেক্সের দ্বৈত-ধার স্বভাবের দিকে মন দিন - এগুলি শক্তিশালী কিন্তু বিস্তৃত ডাটার জন্য জটিল এবং ধীর হতে পারে।

## আরও দেখুন
- মাইক্রোসফটের রেজেক্স ডকুমেন্টেশন: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
- Regex101 (রেজেক্স প্যাটার্ন পরীক্ষা করতে): https://regex101.com/
- অটোমাটা তত্ত্বে পরিচয়: https://en.wikipedia.org/wiki/Automata_theory
