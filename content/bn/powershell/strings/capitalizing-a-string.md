---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:21.967418-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0993\u09AF\u09BC\
  \u09BE\u09B0\u09B6\u09C7\u09B2, \u098F\u0995\u099F\u09BF \u09AC\u09B9\u09C1\u09AE\
  \u09C1\u0996\u09C0 \u099F\u09C1\u09B2 \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7, \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\
  \u099C\u09A8 \u09A8\u09BE \u09AA\u09A1\u09BC\u09C7, \u09B8\u09B9\u099C \u09AA\u09A6\
  \u09CD\u09A7\u09A4\u09BF\u09A4\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\
  \u09CD\u09B0\u09BF\u0982 \u0995\u09CD\u09AF\u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\
  \u0987\u099C \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\u09BC\u0964 \u09A8\u09BF\
  \u099A\u09C7\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.253708-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2, \u098F\u0995\
  \u099F\u09BF \u09AC\u09B9\u09C1\u09AE\u09C1\u0996\u09C0 \u099F\u09C1\u09B2 \u09B9\
  \u09BF\u09B8\u09C7\u09AC\u09C7, \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\
  \u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09A8\u09BE \u09AA\u09A1\u09BC\
  \u09C7, \u09B8\u09B9\u099C \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u09A4\u09C7 \u098F\
  \u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09CD\u09AF\
  \u09BE\u09AA\u09BF\u099F\u09BE\u09B2\u09BE\u0987\u099C \u0995\u09B0\u09A4\u09C7\
  \ \u09A6\u09C7\u09AF\u09BC\u0964 \u09A8\u09BF\u099A\u09C7\u09B0 \u0989\u09AA\u09BE\
  \u09AF\u09BC\u09C7 \u0986\u09AA\u09A8\u09BF \u098F\u099F\u09BF \u0995\u09B0\u09A4\
  \u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09AA\u09CD\u09B0\
  \u09A5\u09AE \u0985\u0995\u09CD\u09B7\u09B0 \u09AC\u09A1\u09BC \u09B9\u09BE\u09A4\
  \u09C7\u09B0 \u0995\u09B0\u09BE"
weight: 2
---

## কিভাবে:
পাওয়ারশেল, একটি বহুমুখী টুল হিসেবে, তৃতীয়-পক্ষের লাইব্রেরির প্রয়োজন না পড়ে, সহজ পদ্ধতিতে একটি স্ট্রিং ক্যাপিটালাইজ করতে দেয়। নিচের উপায়ে আপনি এটি করতে পারেন:

```powershell
# CultureInfo থেকে বিল্ট-ইন .Net মেথড 'ToTitleCase' ব্যবহার করে
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
আউটপুট:
```
Hello world
```

মন্তব্য: এই পদ্ধতি প্রতিটি শব্দের প্রথম অক্ষর ক্যাপিটালাইজ করে। যদি আপনি স্ট্রিং এর কেবল প্রথম অক্ষরটি ক্যাপিটালাইজ করতে চান এবং বাকিটা যেমন আছে তেমনই রাখতে চান, তবে আপনি এই মত কিছু করতে পারেন:

```powershell
# শুধুমাত্র স্ট্রিংয়ের প্রথম অক্ষর ক্যাপিটালাইজ করা
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
আউটপুট:
```
Hello world
```

পাওয়ারশেলে সরাসরি কোনো সিম্পল ফাংশন নেই যা কেবল স্ট্রিংয়ের প্রথম অক্ষর ক্যাপিটালাইজ করে, কিন্তু মৌলিক স্ট্রিং ম্যানিপুলেশন পদ্ধতি যেমন `Substring(0,1).ToUpper()` এবং সংযোজন কম্বাইন করে, আমরা সহজেই কাঙ্খিত ফলাফল অর্জন করতে পারি।
