---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:55.334368-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09AA\u09A8\u09BE\u09B0\
  \ C# \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7 \u098F\u0995\u099F\u09BF REPL \u099A\
  \u09BE\u09B2\u09C1 \u0995\u09B0\u09C1\u09A8 \u09B8\u09BF# \u0987\u09A8\u09CD\u099F\
  \u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09BF\u09AD \u0989\u0987\u09A8\
  \u09CD\u09A1\u09CB \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  \ \u0985\u09A5\u09AC\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\
  \u09AE\u09BF\u09A8\u09BE\u09B2\u09C7 `dotnet-script` \u09B0\u09BE\u09A8 \u0995\u09B0\
  \u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.038955-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09AA\u09A8\u09BE\u09B0 C# \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7\
  \ \u098F\u0995\u099F\u09BF REPL \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09C1\u09A8\
  \ \u09B8\u09BF# \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u200C\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09BF\u09AD \u0989\u0987\u09A8\u09CD\u09A1\u09CB \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u0985\u09A5\u09AC\u09BE \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2\u09C7\
  \ `dotnet-script` \u09B0\u09BE\u09A8 \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09BE\u09B0 \u098F\u0995 \u0985\u09A8\u09C1\u09A7\u09BE\u09AC\u09A8\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
আপনার C# পরিবেশে একটি REPL চালু করুন সি# ইন্টার‌্যাক্টিভ উইন্ডো ব্যবহার করে অথবা আপনার টার্মিনালে `dotnet-script` রান করুন। এখানে এটি ব্যবহার করার এক অনুধাবন দেওয়া হল:

```csharp
> var greeting = "হ্যালো, রিপ্ল!";
> Console.WriteLine(greeting);
হ্যালো, রিপ্ল!
> 
```

আপনি সঙ্গে সঙ্গে প্রতিক্রিয়া পান। কোনো কম্পাইল এবং রান ধাপ নেই। শুধু কোড করুন এবং দেখুন।

## গভীর ডুব
REPL লিস্প থেকে আধুনিক ভাষায় জার্নি করেছে, ডায়নামিক ভাষা যেমন Python-এ বৃদ্ধি পেয়েছে। C#-এর ক্ষেত্রে, Roslyn-এর মাধ্যমে REPL ডেভেলপারদের আরও নিকটে এসেছে। `csi` Roslyn-এর জন্য, এবং `.NET Core`-এর জন্য `dotnet-script`, সলিড অপশন হিসেবে পরিচিত। একটি গভীর কাট: এগুলি কোড লাইন অনুযায়ী, একসাথে নয়, মূল্যায়ন করে, যা সাধারণ C# অ্যাপের বিপরীতে একটি ভিন্ন ক্রিয়ান্বয়ন মডেল। এটি প্রথমে পাস হওয়ার পরের নির্বাহ এবং ভ্যারিয়েবলের পরিসরে প্রভাব ফেলে।

ভিজুয়াল স্টুডিওর সি# ইন্টার‌্যাক্টিভ উইন্ডো একটি রিপ্ল যা Roslyn-এ চালিত। এতে ইন্টেলিসেন্স, একাধিক রেফারেন্স, এবং NuGet প্যাকেজ সমর্থন রয়েছে। প্রারম্ভিক কমান্ড লাইন পরীক্ষার তুলনায় বেশ উন্নতি।

বিকল্প ভাষাগুলির জন্য, Python `IDLE` ব্যব�
