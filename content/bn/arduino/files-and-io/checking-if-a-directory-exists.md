---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:32.314016-06:00
description: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09CD\u09B0\u09B8\u0999\u09CD\u0997\u09C7\
  , \u098F\u0995\u099F\u09BF \u098F\u09B8\u09A1\u09BF \u0995\u09BE\u09B0\u09CD\u09A1\
  \ \u0985\u09A5\u09AC\u09BE \u0985\u09A8\u09C1\u09B0\u09C2\u09AA \u09B8\u09CD\u099F\
  \u09CB\u09B0\u09C7\u099C \u09AE\u09A1\u09BF\u0989\u09B2\u09C7 \u0995\u09CB\u09A8\
  \u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0985\u09B8\u09CD\
  \u09A4\u09BF\u09A4\u09CD\u09AC \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\
  \ \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\
  \u09BE \u09AC\u09BE \u09B2\u09C7\u0996\u09BE\u0995\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.337093-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09CD\u09B0\u09B8\u0999\u09CD\u0997\u09C7\
  , \u098F\u0995\u099F\u09BF \u098F\u09B8\u09A1\u09BF \u0995\u09BE\u09B0\u09CD\u09A1\
  \ \u0985\u09A5\u09AC\u09BE \u0985\u09A8\u09C1\u09B0\u09C2\u09AA \u09B8\u09CD\u099F\
  \u09CB\u09B0\u09C7\u099C \u09AE\u09A1\u09BF\u0989\u09B2\u09C7 \u0995\u09CB\u09A8\
  \u09CB \u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0985\u09B8\u09CD\
  \u09A4\u09BF\u09A4\u09CD\u09AC \u0986\u099B\u09C7 \u0995\u09BF\u09A8\u09BE \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE \u09A4\u09CD\u09B0\u09C1\u099F\u09BF\
  \ \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\
  \u09BE \u09AC\u09BE \u09B2\u09C7\u0996\u09BE\u0995\u09C7\u2026"
title: "\u09A1\u09BF\u09B0\u09C7\u0995\u09CD\u099F\u09B0\u09BF \u0986\u099B\u09C7\
  \ \u0995\u09BF\u09A8\u09BE \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
আরডুইনো প্রোগ্রামিং প্রসঙ্গে, একটি এসডি কার্ড অথবা অনুরূপ স্টোরেজ মডিউলে কোনো ডিরেক্টরি অস্তিত্ব আছে কিনা যাচাই করা ত্রুটি ছাড়াই ফাইল পড়া বা লেখাকে সম্ভব করে। এই অপারেশনটি ডেটা লগিং, কনফিগারেশন ম্যানেজমেন্ট বা যে কোনো কাজে যেখানে গঠনমূলক ফাইল স্টোরেজের প্রয়োজন, আপনার অ্যাপ্লিকেশনে নির্ভরযোগ্যতা এবং সচল পারফরম্যান্স নিশ্চিত করে।

## কিভাবে:
আরডুইনো সরাসরি জটিল ফাইল সিস্টেম অপারেশন সমর্থন করে না। তবে, এসডি লাইব্রেরির ব্যবহার সাহায্যে, যা স্ট্যান্ডার্ড আরডুইনো IDE এর একটি অংশ, আপনি সহজেই ফাইল এবং ডিরেক্টরিগুলি নিয়ে কাজ করতে পারেন। একটি ডিরেক্টরি অস্তিত্ব আছে কিনা যাচাই করতে, আপনাকে প্রথমে এসডি কার্ডটি চালু করে এবং তারপর এসডি লাইব্রেরি থেকে `exists()` মেথড ব্যবহার করতে হবে।

প্রথমে, এসডি লাইব্রেরি অন্তর্ভুক্ত করুন এবং চিপ সিলেক্ট পিন ঘোষণা করুন:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // এসডি কার্ড মডিউলের জন্য চিপ সিলেক্ট পিন
```

আপনার `setup()` ফাংশনে, এসডি কার্ডটি চালু করুন এবং ডিরেক্টরি আছে কিনা যাচাই করুন:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Initialization failed!");
    return;
  }

  // যাচাই করুন যে ডিরেক্টরি আছে কিনা
  if (SD.exists("/myDir")) {
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory doesn't exist.");
  }
}
```
`loop()` ফাংশনে, আপনি এটি খালি রাখতে পারেন বা প্রয়োজন অনুযায়ী অন্যান্য অপারেশনাল কোড যুক্ত করতে পারেন:

```cpp
void loop() {
  // অপারেশনাল কোড যুক্ত করুন অথবা খালি রাখুন 
}
```

কোড চালানোর পর নমুনা আউটপুট হবে একটি অবস্থানে:

```
Directory exists.
```
অথবা

```
Directory doesn't exist.
```

এসডি কার্ডটি সঠিকভাবে ফরম্যাট করা এবং `/myDir` ডিরেক্টরি পাথটি আপনার নির্দিষ্ট চাহিদাগুলির সাথে মেলে এমন নিশ্চিত করা গুরুত্বপূর্ণ। এসডি কার্ডে ফাইল এবং ডিরেক্টরিগুলি সম্পর্কে আরো জটিল অপারেশন সম্পাদনের জন্য এই মৌলিক যাচাইটি একটি মূল ভিত্তি।
