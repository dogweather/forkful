---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:16.818664-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u098F\
  \ \u09AF\u09BE\u09A6\u09C1\u0998\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0989\
  \u09A4\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE \u0985\u09AA\u09CD\u09B0\
  \u09C7\u09A1\u09BF\u0995\u09CD\u099F\u09C7\u09AC\u09B2 \u09A8\u09BF\u0989\u09AE\u09C7\
  \u09B0\u09BF\u0995\u09CD\u09AF\u09BE\u09B2 \u09AE\u09BE\u09A8 \u0989\u09CE\u09AA\
  \u09A8\u09CD\u09A8 \u0995\u09B0\u09BE \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\
  \u09BF\u09A4, \u09AF\u09BE \u09B8\u09BF\u09AE\u09C1\u09B2\u09C7\u09B6\u09A8, \u0997\
  \u09C7\u09AE\u09B8, \u0985\u09A5\u09AC\u09BE \u09B8\u09BF\u0995\u09BF\u0989\u09B0\
  \u09BF\u099F\u09BF \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\
  \u09A8\u09B8 \u098F\u09B0 \u09AE\u09A4\u09CB\u2026"
lastmod: '2024-03-17T18:47:44.174802-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u098F\
  \ \u09AF\u09BE\u09A6\u09C1\u0998\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0989\
  \u09A4\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09BE \u0985\u09AA\u09CD\u09B0\
  \u09C7\u09A1\u09BF\u0995\u09CD\u099F\u09C7\u09AC\u09B2 \u09A8\u09BF\u0989\u09AE\u09C7\
  \u09B0\u09BF\u0995\u09CD\u09AF\u09BE\u09B2 \u09AE\u09BE\u09A8 \u0989\u09CE\u09AA\
  \u09A8\u09CD\u09A8 \u0995\u09B0\u09BE \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\
  \u09BF\u09A4, \u09AF\u09BE \u09B8\u09BF\u09AE\u09C1\u09B2\u09C7\u09B6\u09A8, \u0997\
  \u09C7\u09AE\u09B8, \u0985\u09A5\u09AC\u09BE \u09B8\u09BF\u0995\u09BF\u0989\u09B0\
  \u09BF\u099F\u09BF \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\u09C7\u09B6\
  \u09A8\u09B8 \u098F\u09B0 \u09AE\u09A4\u09CB\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

প্রোগ্রামিং এ যাদুঘর সংখ্যা উত্পাদন করা অপ্রেডিক্টেবল নিউমেরিক্যাল মান উৎপন্ন করা সম্পর্কিত, যা সিমুলেশন, গেমস, অথবা সিকিউরিটি অ্যাপ্লিকেশনস এর মতো বিভিন্ন উদ্দেশ্যে ব্যবহৃত হতে পারে। প্রোগ্রামাররা তাদের প্রকল্পে অনিশ্চয়তার একটি উপাদান প্রবর্তন অথবা বাস্তব জীবনের ভেরিয়েবিলিটি অনুকরণ করার জন্য এই বৈশিষ্ট্যটি ব্যবহার করা হয়।

## কিভাবে:

Lua-তে যাদুঘর সংখ্যা উত্পাদনের জন্য বিল্ট-ইন সাপোর্ট রয়েছে যা `math.random` ফাংশনের মাধ্যমে করা যায়। যে কাঙ্ক্ষিত আউটপুট নির্ভর করে, এই ফাংশনটি একাধিক উপায়ে ব্যবহৃত হতে পারে:

1. **0 এবং 1 এর মধ্যে একটি যাদুঘর ফ্লোটিং-পয়েন্ট সংখ্যা উত্পাদন:**

```Lua
print(math.random())
```

নমুনা আউটপুট হতে পারে `0.13117647051304`। প্রতিটি রান ভিন্ন মান উৎপন্ন করে।

2. **নির্দিষ্ট রেঞ্জের মধ্যে যাদুঘর পূর্ণাঙ্ক উত্পাদন:**

দুই সীমানার মধ্যে একটি যাদুঘর পূর্ণাঙ্ক উত্পাদনের জন্য, আপনাকে প্রথমে `math.randomseed(os.time())` ব্যবহার করে সীড সেট করতে হবে ভেরিয়েবিলিটির জন্য, তারপর দুটি আর্গুমেন্টের সাথে `math.random` কল করতে হবে:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- 1 এবং 10 এর মধ্যে একটি যাদুঘর পূর্ণাঙ্ক উত্পাদন করে
```

নমুনা আউটপুট হতে পারে `7`। আবার, প্রতিবার আউটপুট ভিন্ন হবে।

`math.randomseed` ব্যবহার করে সীড সেট করা এই কারণে গুরুত্বপূর্ণ কারণ এটি ছাড়া, `math.random` প্রতি প্রোগ্রামের রানে একই সংখ্যার ধারা উত্পাদন করতে পারে। সাধারণত, বর্তমান সময়, `os.time()`, প্রতি নির্বাহে ভিন্ন ধারা নিশ্চিত করে।

## গভীর ডুব

Lua-তে (এবং অধিকাংশ প্রোগ্রামিং ভাষায়) যাদুঘর সংখ্যা উত্পাদনের অধীনে যে পদ্ধতি আছে তা সত্যিই যাদুঘর নয় বরং পিউডোর‍্যান্ডম, যা অ্যালগরিদম দ্বারা উৎপাদিত হয়। এই পিউডোর‍্যান্ডম নাম্বার জেনারেটারস (PRNGs) হল নির্ধারক এবং সংখ্যার ধারা উৎপাদন শুরু করার জন্য একটি সীড মূল্যের প্রয়োজন। যাদুঘরতা মানের জন্য সীডিং এর চয়ন খুব উল্লেখযোগ্য, যা বর্তমান সময় ব্যবহার একটি সাধারণ অনুশীলন।

ঐতিহাসিকভাবে, Lua-র যাদুঘর সংখ্যা উৎপাদন ক্ষমতা বিকশিত হয়েছে। পূর্ববর্তী সংস্করণগুলি C স্ট্যান্ডার্ড লাইব্রেরির `rand()` ফাংশনের উপর নির্ভর করত, যা ইমপ্লিমেন্টেশনগুলিতে মান এবং পারফর্মেন্সে ভিন্ন ছিল। Lua-র বর্তমান সংস্করণ এটি উন্নত করেছে যেটি অন্তর্নিহিত প্ল্যাটফর্মের উপর নির্ভর করে আরো শক্তিশালী পদ্ধতি ব্যবহার করে, যাদুঘর সংখ্যা উৎপাদনের জন্য বৃহত্তর সুসংগতি এবং উপযোগিতা অফার করছে।

যাদুঘর স্তরের নিরাপত্তার জন্য প্রজেক্টগুলি প্রয়োজন হলে, বিল্ট-ইন Lua ফাংশন্যালিটি পর্যাপ্ত নাও হতে পারে কারণ PRNGs-এর নির্ধারক প্রকৃতি। এই ধরনের ক্ষেত্রে, প্রোগ্রামাররা প্রায়ই বাহ্যিক লাইব্রেরিগুলি অথবা সিস্টেম-নির্দিষ্ট APIs-এ মোড়ানো যা উচ্চ-নিরাপত্তা অ্যাপ্লিকেশনের জন্য উপযুক্ত অনির্ধারক যাদুঘর সংখ্যা সরবরাহ করতে পারে।
