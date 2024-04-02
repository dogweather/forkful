---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:19.908251-06:00
description: "TOML \u09B9\u09B2 Tom's Obvious, Minimal Language \u098F\u09B0 \u09B8\
  \u0982\u0995\u09CD\u09B7\u09C7\u09AA, \u09AF\u09BE \u098F\u0995\u099F\u09BF \u0995\
  \u09A8\u09AB\u09BF\u0997\u09C1\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\
  \ \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\
  \u09AA\u09B7\u09CD\u099F \u09B8\u09C7\u09AE\u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09CD\
  \u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\
  \u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.061918-06:00'
model: gpt-4-0125-preview
summary: "TOML \u09B9\u09B2 Tom's Obvious, Minimal Language \u098F\u09B0 \u09B8\u0982\
  \u0995\u09CD\u09B7\u09C7\u09AA, \u09AF\u09BE \u098F\u0995\u099F\u09BF \u0995\u09A8\
  \u09AB\u09BF\u0997\u09C1\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2 \u09AB\
  \u09B0\u09AE\u09CD\u09AF\u09BE\u099F \u09AF\u09BE \u098F\u09B0 \u09B8\u09CD\u09AA\
  \u09B7\u09CD\u099F \u09B8\u09C7\u09AE\u09BE\u09A8\u09CD\u099F\u09BF\u0995\u09CD\u09B8\
  \u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\
  \u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0\u2026"
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি এবং কেন?

TOML হল Tom's Obvious, Minimal Language এর সংক্ষেপ, যা একটি কনফিগুরেশন ফাইল ফরম্যাট যা এর স্পষ্ট সেমান্টিক্সের কারণে পড়া সহজ। প্রোগ্রামাররা সিস্টেমের মধ্যে ডাটা আদান-প্রদান সহজ করার জন্য এবং মানব পঠনীয়তা এবং মেশিন পার্সেবিলিটির মধ্যে একটি সমতুল্য ভারসাম্য তৈরি করার কারণে কনফিগ ফাইলের জন্য এটি ব্যবহার করে।

## কিভাবে:

প্রথমে, `Tomlyn` এর মতো একটি TOML পার্সার ইনস্টল করুন। আপনার প্যাকেজ ম্যানেজার ব্যবহার করুন:

```csharp
dotnet add package Tomlyn
```

পরবর্তী, একটি TOML ফাইল পার্স করুন:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Owner: {tomlTable["owner"]["name"]}");
// আউটপুট:
// Owner: Tom Preston-Werner
```

এখন, TOML তৈরি এবং লেখা:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML config.toml এ লেখা হয়েছে");
// আউটপুট:
// TOML config.toml এ লেখা হয়েছে
```

## গভীর ডাইভ:

TOML টি 2013 সালের দিকে GitHub এর সহ-প্রতিষ্ঠাতা, Tom Preston-Werner কর্তৃক তৈরি করা হয়েছিল, YAML এবং JSON এর মতো বিদ্যমান ফর্ম্যাটের সীমাবদ্ধতার বিপরীতে কনফিগারেশন সেটিংসে একটি প্রতিক্রিয়া হিসেবে। এটি বিশেষত কনফিগগুলির জন্য সরল এবং অস্পষ্টতামুক্ত থাকার উপর জোর দেয়।

বিকল্প কনফিগ ফর্ম্যাটে রয়েছে YAML, JSON, এবং XML। তবে, TOML মানুষের জন্য আরও বন্ধুত্বপূর্ণ হিসেবে প্রকাশ পায়, বিশেষ করে কনফিগারেশন ফাইলের ক্ষেত্রে যেখানে হাতে সম্পাদনা সাধারণ। JSON, যদিও ব্যাপকভাবে ব্যবহৃত, জটিল কনফিগের জন্য কম পঠনীয় এবং XML বাগাঢ়াবাজ। YAML, যদিও পঠনীয়তায় অনুরূপ, ধূসর স্থানের ভারী ব্যবহার এবং নির্দিষ্ট সামগ্রীর সাথে নিরাপত্তা ঝুঁকিতে জটিল হতে পারে।

বাস্তবায়নের দিক দিয়ে, TOML একটি হ্যাশ টেবিলে পরিষ্কারভাবে ম্যাপিং করার উপর মনোনিবেশ করে, ডেটা এক্স
