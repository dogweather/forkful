---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:04.037529-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u09B9\
  \u09BE\u09A4 \u0997\u09C1\u099F\u09BF\u09DF\u09C7 \u0995\u09BF\u099B\u09C1 \u0995\
  \u09CB\u09A1\u09C7\u09B0 \u0989\u09AA\u09B0 \u09B9\u09BE\u09A4 \u09A6\u09BF\u0987\
  \u0964 \u09A7\u09B0\u09C7 \u09A8\u09BF\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\
  \u09BE\u099B\u09C7 .NET 6 \u0985\u09A5\u09AC\u09BE \u09A4\u09BE\u09B0 \u09AA\u09B0\
  \u09C7\u09B0 \u09AD\u09BE\u09B0\u09CD\u09B8\u09A8 \u0986\u099B\u09C7 - \u09B2\u09C7\
  \u0996\u09BE\u09B0 \u09B8\u09AE\u09DF \u098F\u099F\u09BE\u0987 \u09B8\u09B0\u09CD\
  \u09AC\u09B6\u09C7\u09B7\u0964 \u0986\u09AA\u09A8\u09BF \u098F\u0987 \u099C\u09A8\
  \u09CD\u09AF\u09C7 .NET CLI\u2026"
lastmod: '2024-03-17T18:47:44.038000-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u09B9\u09BE\u09A4 \u0997\u09C1\u099F\u09BF\u09DF\
  \u09C7 \u0995\u09BF\u099B\u09C1 \u0995\u09CB\u09A1\u09C7\u09B0 \u0989\u09AA\u09B0\
  \ \u09B9\u09BE\u09A4 \u09A6\u09BF\u0987\u0964 \u09A7\u09B0\u09C7 \u09A8\u09BF\u09A8\
  \ \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 .NET 6 \u0985\u09A5\u09AC\
  \u09BE \u09A4\u09BE\u09B0 \u09AA\u09B0\u09C7\u09B0 \u09AD\u09BE\u09B0\u09CD\u09B8\
  \u09A8 \u0986\u099B\u09C7 - \u09B2\u09C7\u0996\u09BE\u09B0 \u09B8\u09AE\u09DF \u098F\
  \u099F\u09BE\u0987 \u09B8\u09B0\u09CD\u09AC\u09B6\u09C7\u09B7\u0964 \u0986\u09AA\
  \u09A8\u09BF \u098F\u0987 \u099C\u09A8\u09CD\u09AF\u09C7 .NET CLI \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8\u0964\n\n\u098F\u0995\u099F\
  \u09BF \u09A8\u09A4\u09C1\u09A8 \u0995\u09A8\u09B8\u09CB\u09B2 \u0985\u09CD\u09AF\
  \u09BE\u09AA \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09C1\u09A8."
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কিভাবে:
চলুন হাত গুটিয়ে কিছু কোডের উপর হাত দিই। ধরে নিন আপনার কাছে .NET 6 অথবা তার পরের ভার্সন আছে - লেখার সময় এটাই সর্বশেষ। আপনি এই জন্যে .NET CLI ব্যবহার করবেন।

একটি নতুন কনসোল অ্যাপ তৈরি করুন:
```C#
dotnet new console -o MyNewProject
```
আপনার প্রজেক্ট ডিরেক্টরিতে প্রবেশ করুন:
```C#
cd MyNewProject
```
আপনার নতুন, বয়লারপ্লেট হ্যালো ওয়ার্ল্ড চালান:
```C#
dotnet run
```
আপনি দেখতে পাবেন:
```
হ্যালো, ওয়ার্ল্ড!
```
আপনার নতুন প্রজেক্ট আকাশে উঠে গেছে!

## গভীর ডাইভ
অতীতে, আপনি সম্ভবত Visual Studio চালু করে একটি উইজার্ডের মাধ্যমে ক্লিক করে যেতেন। এখন আর নয় - এখন ডটনেট CLI হল যেতে হবে। এটি দ্রুত এবং আপনার ডেভ পরিবেশ সম্পর্কে অনেক ধারণা করে না।

বিকল্প? অবশ্যই। Visual Studio এখনো GUI অভিজ্ঞতার জন্য প্রস্তুত। Rider এবং Visual Studio Code ভালো বিকল্প। কিন্তু CLI? এটি সব সম্পর্কে একটি পাতলা, মিনমিনে স্ক্রিপ্টিং অনুভূতি।

বাস্তবায়নের বিস্তারিত? আপনার `.csproj` ফাইল রাজ্যের চাবিকাঠি ধরে রাখে। এটি XML, কিন্তু চিন্তা করবেন না - এটি প্রায় নিজে নিজে মানিয়ে নেয়। এখানে আপনার বিল্ড প্রক্রিয়ার প্রয়োজনীয় তথ্য রয়েছে - টার্গেট ফ্রেমওয়ার্ক, ডিপেনডেন্সি, প্রজেক্ট রেফারেন্স, সব ভালো জিনিস।

## আরও দেখুন
- [অফিসিয়াল .NET CLI ডকুমেন্টেশন](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- [Visual Studio পণ্য পাতা](https://visualstudio.microsoft.com/)
- [.NET প্রজেক্ট SDK ওভারভিউ](https://docs.microsoft.com/en-us/dotnet/core/project-sdk/overview)
