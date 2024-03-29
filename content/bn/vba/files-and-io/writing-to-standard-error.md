---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:44:14.112495-06:00
description: "Visual Basic for Applications (VBA) \u098F \u09AE\u09BE\u09A8\u0995\
  \ \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B2\u09C7\
  \u0996\u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u09A8\u09BF\u09DF\u09AE\u09BF\u09A4\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F \u09A5\u09C7\u0995\u09C7 \u09A4\u09CD\u09B0\
  \u09C1\u099F\u09BF \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09AC\u09BE \u09A1\u09BE\
  \u09DF\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\u09CD\u09B8 \u09AA\u09C3\u09A5\
  \u0995 \u0995\u09B0\u09C7 \u09AE\u09C2\u09B2\u09A4 \u0995\u09A8\u09B8\u09CB\u09B2\
  \ \u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.876009-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications (VBA) \u098F \u09AE\u09BE\u09A8\u0995 \u09A4\
  \u09CD\u09B0\u09C1\u099F\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B2\u09C7\u0996\
  \u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09C7\u09B0 \u09A8\u09BF\u09DF\u09AE\u09BF\u09A4 \u0986\
  \u0989\u099F\u09AA\u09C1\u099F \u09A5\u09C7\u0995\u09C7 \u09A4\u09CD\u09B0\u09C1\
  \u099F\u09BF \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE \u09AC\u09BE \u09A1\u09BE\u09DF\
  \u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995\u09CD\u09B8 \u09AA\u09C3\u09A5\u0995\
  \ \u0995\u09B0\u09C7 \u09AE\u09C2\u09B2\u09A4 \u0995\u09A8\u09B8\u09CB\u09B2 \u09AC\
  \u09BE\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি এবং কেন?

Visual Basic for Applications (VBA) এ মানক ত্রুটির সাথে লেখার অর্থ হল প্রোগ্রামের নিয়মিত আউটপুট থেকে ত্রুটি বার্তা বা ডায়াগনস্টিক্স পৃথক করে মূলত কনসোল বা লগ ফাইলে নির্দেশ করে। প্রোগ্রামাররা এটি করে থাকেন নিয়মিত প্রোগ্রাম আউটপুট থেকে ত্রুটি বার্তা পৃথক করে ট্রাবলশুটিং করা অথবা মূল আউটপুটের সাথে ঝামেলা ছাড়াই ব্যবহারকারীদের কোন সমস্যার ব্যাপারে সতর্ক করতে সুবিধাজনক করতে।

## কিভাবে:

VBA তে, অন্যান্য প্রোগ্রামিং ভাষার মতো সুনির্দিষ্ট ভাবে মানক ত্রুটিতে লেখার জন্য সরাসরি নির্মিত ফাংশন না থাকায়, একটি সাধারণ প্রতিকার হল `Debug.Print` ব্যবহার করে ডেভেলপমেন্ট ত্রুটি আউটপুটের জন্য অথবা উৎপাদন অ্যাপ্লিকেশনের জন্য এই আচরণের অনুকরণ করে একটি কাস্টম লগিং ফাংশন তৈরি করা। নিচে এমন একটি ফাংশন কিভাবে বাস্তবায়ন ও ব্যবহার করতে পারেন তার একটি উদাহরণ দেওয়া হল:

```vb
Sub WriteToErrorLog(msg As String)
    ' মানক ত্রুটিতে লেখার জন্য কাস্টম ফাংশন অনুকরণ
    ' বাস্তব ডেপ্লয়মেন্টে, এটি একটি পৃথক লগ ফাইল বা একটি উৎসর্গীকৃত ডিবাগিং উইন্ডোতে লিখতে পারে
    Open "ErrorLog.txt" For Append As #1 ' "ErrorLog.txt" কে আপনার কাঙ্ক্ষিত লগ ফাইলের পথে পরিবর্তন করুন
    Print #1, "ERROR: " & msg
    Close #1
    Debug.Print "ERROR: " & msg ' ডেভেলপারের ডিবাগিং এর জন্য IDE এর তাৎক্ষণিক উইন্ডোতেও আউটপুট
End Sub

Sub Demonstration()
    ' WriteToErrorLog ফাংশনের উদাহরণ ব্যবহার
    WriteToErrorLog "আপনার অনুরোধ প্রক্রিয়া করার সময় একটি ত্রুটি ঘটেছে।"
End Sub
```

"ErrorLog.txt" এ নমুনা আউটপুট এরকম দেখাতে পারে:
```
ERROR: আপনার অনুরোধ প্রক্রিয়া করার সময় একটি ত্রুটি ঘটেছে।
```

এবং VBA IDE তাৎক্ষণিক উইন্ডোতে:
```
ERROR: আপনার অনুরোধ প্রক্রিয়া করার সময় একটি ত্রুটি ঘটেছে।
```

## গভীর ডুব

Visual Basic for Applications স্বাভাবিকভাবে মানক ত্রুটিতে লেখার জন্য একটি উৎসর্গীকৃত পদ্ধতি অন্তর্ভুক্ত করে না, এর কারণ হল এর এক্সেল, ওয়ার্ড বা অ্যাক্সেসের মতো হোস্ট অ্যাপ্লিকেশনগুলির সাথে গভীরভাবে সম্পৃক্ত প্রকৃতি, যা সাধারণত কনসোল আউটপুটের চেয়ে গ্রাফিকাল ইউজার ইন্টারফেসের উপর নির্ভর করে। এটি সি বা পাইথনের মতো ভাষাতে বিকাশিত কনসোল-ভিত্তিক অ্যাপ্লিকেশনগুলির থেকে একটি উল্লেখযোগ্য বিচ্যুতি, যেখানে মানক আউটপুট এবং মানক ত্রুটি স্ট্রিমগুলি মৌলিক ধারণা হয়।

ঐতিহাসিকভাবে, VBA এর দৃষ্টি সবসময় তার হোস্ট অ্যাপ্লিকেশনের নথি মডেলের সাথে মিশে থাকা এবং ঐতিহ্যবাহী অ্যাপ্লিকেশন লগিং পদ্ধতির থেকে কম উপর মনোনিবেশ করে। সুতরাং, ডেভেলপাররা প্রায়শই উদাহরণে দেখানো কাস্টম লগিং সমাধানগুলির বাস্তবায়নের দিকে ঝুঁকে পড়েন, বা আরও উন্নত ত্রুটি হ্যান্ডলিং ও লগিং প্রয়োজনের জন্য উইন্ডোজ API কলগুলি ব্যবহার করেন।

যদিও প্রদর্শিত পদ্ধতি একটি প্রতিকার সরবরাহ করে, আরও দৃঢ় লগিং এবং ত্রুটি হ্যান্ডলিং খুঁজছে ডেভেলপাররা বাইরের সিস্টেম বা লাইব্রেরিগুলির সাথে একীভূত হওয়ার অন্বেষণ করতে পারে, যা আরও সোফিস্টিকেটেড লগিং সক্ষম। আধুনিক ডেভেলপমেন্টে, বিশেষ করে ডিবাগিং এবং রক্ষণাবেক্ষণে মনোনিবেশ করে, স্পষ্ট, প্রাসঙ্গিক, এবং পৃথক মানক এবং ত্রুটি আউটপুটের লগিং এর গুরুত্ব অত্যন্ত জোরালো বোঝানো যায়, অনেকেই VBA এর মূল ক্ষমতা ছাড়িয়ে সমাধানের সন্ধান করে।
