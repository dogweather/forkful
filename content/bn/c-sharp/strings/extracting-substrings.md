---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:28.357701-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C# \u098F \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09B8\u09BE\u09AC\u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u099F\u09C7\u09A8\u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\
  \u09BE \u09B8\u09B9\u099C\u0964 \u098F\u0996\u09BE\u09A8\u09C7 `Substring` \u09AE\
  \u09C7\u09A5\u09A1 \u098F\u09AC\u0982 \u09B0\u09C7\u099E\u09CD\u099C \u0985\u09AA\
  \u09BE\u09B0\u09C7\u099F\u09B0\u09B0\u09CD\u09B8 \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\
  \u09AF\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B8\u09CD\u09B2\u09BE\
  \u0987\u09B8\u09BF\u0982 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09BE\
  \ \u09B9\u09AF\u09BC \u09A4\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4\u2026"
lastmod: '2024-03-17T18:47:44.026266-06:00'
model: gpt-4-0125-preview
summary: "C# \u098F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7\
  \ \u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099F\u09C7\u09A8\
  \u09C7 \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE \u09B8\u09B9\u099C\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 `Substring` \u09AE\u09C7\u09A5\u09A1 \u098F\u09AC\u0982 \u09B0\
  \u09C7\u099E\u09CD\u099C \u0985\u09AA\u09BE\u09B0\u09C7\u099F\u09B0\u09B0\u09CD\u09B8\
  \ \u09B8\u09BE\u09B9\u09BE\u09AF\u09CD\u09AF\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09B8\u09CD\u09B2\u09BE\u0987\u09B8\u09BF\u0982 \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC \u09A4\u09BE\u09B0 \u098F\
  \u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u09A6\u09C3\u09B7\u09CD\u099F\
  \u09BF\u09AA\u09BE\u09A4."
title: "\u09B8\u09BE\u09AC\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AC\u09C7\u09B0\
  \ \u0995\u09B0\u09BE"
weight: 6
---

## কিভাবে:
C# এ স্ট্রিং থেকে সাবস্ট্রিং টেনে বের করা সহজ। এখানে `Substring` মেথড এবং রেঞ্জ অপারেটরর্স সাহায্যে স্ট্রিং স্লাইসিং কিভাবে করা হয় তার একটি দ্রুত দৃষ্টিপাত:

```C#
string fullString = "Hello, World! Life is beautiful.";
// Substring(startIndex, length) ব্যবহার করে
string extracted1 = fullString.Substring(7, 5); // "World"

Console.WriteLine(extracted1); // আউটপুট: World

// রেঞ্জ অপারেটর [..] সাহায্যে স্ট্রিং স্লাইসিং ব্যবহার করে
string extracted2 = fullString[13..24]; // "Life is beau"

Console.WriteLine(extracted2); // আউটপুট: Life is beau
```

## গভীর ডাইভ
সাবস্ট্রিং নতুন কোন কৌশল নয়। এগুলো C এবং জাভা ভাষায় বহু যুগ ধরে আছে। তবে, C# এ পদ্ধতি এবং ফিচারগুলো সাথে নিয়ে এসেছে যা পঠনীয়তা এবং ব্যবহারের সহজতা প্রাধান্য দেয়।

ঐতিহাসিকভাবে, প্রোগ্রামাররা লুপস এবং সূক্ষ্ম ইনডেক্স গণনা ব্যবহার করত। C# এর `Substring` মেথড হল দারুন এক আপগ্রেড। এটি সোজা—এটি একটি শুরুর ইনডেক্স এবং ঐচ্ছিকভাবে, একটি দৈর্ঘ্য প্রবেশ করাও, এবং এটি আপনার জন্য স্লাইসিং করে দেয়।

দৃশ্যসম্ভার শেষ হয় না এখানেই। C# 8.0 এবং তারপরে, আমরা রেঞ্জ অপারেটর [..] যেমন নির্দিষ্ট বিশেষতাগুলি সাথে পরিচিত হয়েছি। এগুলো আরও প্রাকৃতিক স্লাইসিং এক্সপ্রেশনের অনুমতি দেয়, বিশেষ করে যখন স্ট্রিংয়ের শেষ থেকে সাপেক্ষে ইনডেক্স ব্যবহার করা হয় (`^` অপারেটর দ্বারা নির্দেশিত)।

`Substring` এর বিকল্পের মধ্যে রয়েছে `Split`, Regex অপারেশন্স, অথবা LINQ সাহায্যে স্ট্রিং ম্যানিপুলেশন। পছন্দ সিচুয়েশনের উপর নির্ভর করে—আপনি হয়তো একটি CSV লাইন বিভাজন করতে পারেন, Regex একটি প্যাটার্ন, অথবা LINQ এক্সপ্রেশন সাহায্যে উপাদানগুলোকে বাছাই করতে পারেন।

বাস্তবায়নের দিক থেকে, C# স্ট্রিংগুলি অমূবল্য। আপনি যখন একটি সাবস্ট্রিং নিচ্ছেন, আপনি মূলটিকে পরিবর্তন করছেন না। বরং, আপনি একটি নতুন স্ট্রিং তৈরি করছেন যা কিছু মূলের মেমরি স্পেস শেয়ার করে — আপনি যখন এটি পরিবর্তন করবেন, তখন এটি নিজস্ব মেমরি বরাদ্দের জন্য যায়।

## আরো দেখুন
আপনি যদি গভীরে ডুব দিতে চান বা সম্পর্কিত বিষয়গুলি অন্বেষণ করতে চান, এখানে কিছু সম্পদ রয়েছে:
- `Substring` সম্পর্কে Microsoft-এর অফিসিয়াল ডকুমেন্টেশন: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- রেঞ্জ অপারেটর এবং ইন্ডিসেস সম্পর্কে C#-এ আরো: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-8.0/ranges
- LINQ সাহায্যে স্ট্রিং ম্যানিপুলেশন: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/
- C#-এ রেগুলার এক্সপ্রেশনস: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
