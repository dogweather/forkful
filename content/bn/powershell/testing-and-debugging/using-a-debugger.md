---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:26.210775-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09BE\u0993\u09AF\u09BC\
  \u09BE\u09B0\u09B6\u09C7\u09B2\u09C7, \u0986\u09AA\u09A8\u09BF \u09A8\u09BF\u09B0\
  \u09CD\u09AE\u09BF\u09A4 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2\
  \ \u0987\u09A8\u09CD\u099F\u09BF\u0997\u09CD\u09B0\u09C7\u099F\u09C7\u09A1 \u09B8\
  \u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09BF\u0982 \u098F\u09A8\u09AD\u09BE\
  \u09AF\u09BC\u09B0\u09A8\u09AE\u09C7\u09A8\u09CD\u099F (ISE) \u0985\u09A5\u09AC\u09BE\
  \ \u09AD\u09BF\u099C\u09C1\u09AF\u09BC\u09BE\u09B2 \u09B8\u09CD\u099F\u09C1\u09A1\
  \u09BF\u0993 \u0995\u09CB\u09A1 (VS \u0995\u09CB\u09A1) \u098F\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2 \u098F\
  \u0995\u09CD\u09B8\u099F\u09C7\u09A8\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.281249-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0993\u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2\u09C7, \u0986\
  \u09AA\u09A8\u09BF \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09AA\u09BE\u0993\
  \u09AF\u09BC\u09BE\u09B0\u09B6\u09C7\u09B2 \u0987\u09A8\u09CD\u099F\u09BF\u0997\u09CD\
  \u09B0\u09C7\u099F\u09C7\u09A1 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F\
  \u09BF\u0982 \u098F\u09A8\u09AD\u09BE\u09AF\u09BC\u09B0\u09A8\u09AE\u09C7\u09A8\u09CD\
  \u099F (ISE) \u0985\u09A5\u09AC\u09BE \u09AD\u09BF\u099C\u09C1\u09AF\u09BC\u09BE\
  \u09B2 \u09B8\u09CD\u099F\u09C1\u09A1\u09BF\u0993 \u0995\u09CB\u09A1 (VS \u0995\u09CB\
  \u09A1) \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\
  \u09B0\u09B6\u09C7\u09B2 \u098F\u0995\u09CD\u09B8\u099F\u09C7\u09A8\u09B6\u09A8\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u09A1\u09BF\u09AC\u09BE\u0997 \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0989\u09AD\u09AF\u09BC\u09C7\u09A4\u09C7 \u09AC\u09CD\u09B0\u09C7\u0995\u09AA\
  \u09AF\u09BC\u09C7\u09A8\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\
  \u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\
  \u09B2\u09CB."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
পাওয়ারশেলে, আপনি নির্মিত পাওয়ারশেল ইন্টিগ্রেটেড স্ক্রিপ্টিং এনভায়রনমেন্ট (ISE) অথবা ভিজুয়াল স্টুডিও কোড (VS কোড) এর সাথে পাওয়ারশেল এক্সটেনশন ব্যবহার করে স্ক্রিপ্ট ডিবাগ করতে পারেন। এখানে উভয়েতে ব্রেকপয়েন্ট ব্যবহারের উপায় দেওয়া হলো:

### পাওয়ারশেল ISE:
```PowerShell
# নির্দিষ্ট লাইনে একটি ব্রেকপয়েন্ট সেট করুন
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# আপনার স্ক্রিপ্ট স্বাভাবিকভাবে চালান
.\MyScript.ps1

# স্ক্রিপ্ট যখন ব্রেকপয়েন্টে পৌঁছায়, আপনি ভেরিয়েবল নিরীক্ষণ করতে পারেন
$myVariable

# এক্সিকিউশন চালিয়ে যান
Continue
```

### ভিজুয়াল স্টুডিও কোড:
```PowerShell
# আপনার পাওয়ারশেল স্ক্রিপ্ট ভিএস কোডে খুলুন।
# লাইন নম্বরের বাম পাশে ক্লিক করে একটি ব্রেকপয়েন্ট সেট করুন।
# ডিবাগিং শুরু করতে F5 চাপুন অথবা 'স্টার্ট ডিবাগিং' এ ক্লিক করুন।

# ভিএস কোড আপনার ব্রেকপয়েন্টে এক্সিকিউশন স্থগিত করবে।
# ডিবাগ প্যানেল ব্যবহার করে ভেরিয়েবল দেখুন, কল স্ট্যাক নিরীক্ষণ করুন, এবং প্রবাহ নিয়ন্ত্রণ করুন।
```

উভয় পরিবেশে ডিবাগিং আপনাকে ডিবাগিংয়ের সময় (F11), উপর দিয়ে পদক্ষেপ (F10), এবং বেরিয়ে আসা (Shift+F11) সম্ভব করে তোলে।

## গভীর ডুব
ঐতিহাসিকভাবে, পাওয়ারশেলে ডিবাগিং একটু জটিল ছিল; ভেরিয়েবলের অবস্থা আউটপুট করতে অনেক `Write-Host` লাইন বা ক্লাসিক ট্রায়াল-এন্ড-এরর পদ্ধতি প্রয়োজন ছিল। পাওয়ারশেল ISE এর আগমনের সাথে, এবং সম্প্রতি ভিএস কোড এর সাথে এর সমৃদ্ধ ডিবাগিং বৈশিষ্ট্যের সাথে, পাওয়ারশেল ডিবাগিং প্রায় পূর্ণ-বৈশিষ্ট্যসম্পন্ন প্রোগ্রামিং ভাষায় যেমন ছিল তেমনই সহজাত হয়ে উঠেছিল।

পাওয়ারশেলের নেটিভ ডিবাগিং টুলের বিকল্পের মধ্যে থার্ড-পার্টি টুলস যেমন PowerGUI অথবা ভিজুয়াল স্টুডিও মতো রোবাস্ট IDEs ব্যবহার করা অন্তর্ভুক্ত রয়েছে যেখানে পাওয়ারশেল প্লাগইন রয়েছে।

একটি ডিবাগার বাস্তবায়ন করার সময়, বিশেষ করে দট-সোর্সড স্ক্রিপ্ট বা মডিউলের সাথে কাজ করার সময় স্ক্রিপ্টের স্কোপ বিবেচনা করুন। ডিবাগিং সেশনের সময় নির্দিষ্ট নিয়ন্ত্রণের জন্য ব্রেকপয়েন্টগুলি শর্তভিত্তিক, ভেরিয়েবল পরিবর্তন-ভিত্তিক, অথবা লাইন-ভিত্তিক হতে পারে।

তাছাড়া, পাওয়ারশেল কোরের (ক্রস-প্ল্যাটফর্ম পাওয়ারশেল) সাথে সংক্রমণের সাথে সাথে, ডিবাগিং মূলত ভিএস কোডের হাতে চলে গেছে, যা বিভিন্ন প্ল্যাটফর্মে একটি ধারাবাহিক অভিজ্ঞতা প্রদান করে।

## আরও দেখুন
পাওয়ারশেলে ডিবাগিং সম্পর্কে আরও জানতে:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
