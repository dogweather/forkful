---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:35.323556-06:00
description: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u099C\u09C7\
  \u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u09B8\u09AE\u09AF\
  \u09BC\u09C7\u09B0 \u09A4\u09A5\u09CD\u09AF \u0985\u09B0\u09CD\u099C\u09A8 \u0995\
  \u09B0\u09BE, \u09AF\u09BE \u09B2\u0997\u09BF\u0982, \u09B8\u09AE\u09AF\u09BC\u09AE\
  \u09CB\u09B9\u09B0\u09A3, \u0985\u09A5\u09AC\u09BE \u0995\u09BE\u099C \u09A8\u09BF\
  \u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\
  \u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF \u09B9\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u0964\u2026"
lastmod: '2024-03-17T18:47:44.333094-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u099C\u09C7\
  \u0995\u09CD\u099F\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 \u09AC\u09B0\u09CD\u09A4\u09AE\
  \u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE\
  \ \u09AE\u09BE\u09A8\u09C7 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC \u09B8\u09AE\u09AF\
  \u09BC\u09C7\u09B0 \u09A4\u09A5\u09CD\u09AF \u0985\u09B0\u09CD\u099C\u09A8 \u0995\
  \u09B0\u09BE, \u09AF\u09BE \u09B2\u0997\u09BF\u0982, \u09B8\u09AE\u09AF\u09BC\u09AE\
  \u09CB\u09B9\u09B0\u09A3, \u0985\u09A5\u09AC\u09BE \u0995\u09BE\u099C \u09A8\u09BF\
  \u09B0\u09CD\u09A7\u09BE\u09B0\u09A3\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\
  \u09AA\u09B0\u09BF\u09B9\u09BE\u09B0\u09CD\u09AF \u09B9\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u0964\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
---

{{< edit_this_page >}}

## কি এবং কেন?
আরডুইনো প্রজেক্টগুলিতে বর্তমান তারিখ পাওয়া মানে বাস্তব সময়ের তথ্য অর্জন করা, যা লগিং, সময়মোহরণ, অথবা কাজ নির্ধারণের জন্য অপরিহার্য হতে পারে। প্রোগ্রামারদের প্রায়ই তাদের আইওটি এবং এমবেডেড প্রকল্পে ফাংশনালিটি বাড়ানো, ডেটার প্রাসঙ্গিকতা নিশ্চিত করা, এবং সময়-সংবেদনশীল অপারেশনগুলি সুবিধাজনক করার জন্য এই সক্ষমতা প্রয়োজন হয়।

## কিভাবে:
আরডুইনো নিজে থেকে সরাসরি বর্তমান তারিখ আনার কোন অন্তর্নির্মিত উপায় নেই, কারণ এতে একটি বাস্তব সময় ঘড়ি (RTC) এর অভাব রয়েছে। যাইহোক, DS3231 এর মতো বাহ্যিক RTC মডিউলগুলি এবং `RTClib` এর মতো লাইব্রেরি ব্যবহার করে, যা আদাফ্রুট দ্বারা বিকাশিত, এইগুলির সাথে ইন্টারফেসের কাজটি সরল করে তুলেছে, এর মাধ্যমে এটা সম্ভব।

প্রথমে, আরডুইনো আইডিইতে `RTClib` লাইব্রেরি ইনস্টল করা আছে কিনা তা নিশ্চিত করুন। এরপর, আপনার RTC মডিউলটি এর ডকুমেন্টেশন অনুযায়ী আরডুইনোর সাথে সংযোগ করুন।

এখানে একটি সহজ উদাহরণ দেওয়া হলো:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // যখন নতুন ডিভাইসে অথবা শক্তি হারানোর পর সময় সেট করা প্রয়োজন, আপনি এখানে সেট করতে পারেন।
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Current Date: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // 3 সেকেন্ডের বিলম্ব করতে যাতে সিরিয়াল স্প্যাম কম হয়
}
```

উদাহরণ আউটপুট (ধরে নেওয়া যাচ্ছে আপনার RTC আগে থেকেই সেট করা হয়েছে):

```
Current Date: 2023/4/15
```

এই কোডে RTC মডিউলটি চালু করা হয় এবং তারপর, লুপে, প্রতি 3 সেকেন্ডে বর্তমান তারিখ আনা হয় এবং সিরিয়াল মনিটরে প্রিন্ট করা হয়। মনে রাখবেন, `rtc.adjust(...)` লাইনটি আনকমেন্ট এবং মডিফাই করা যায় RTC এর তারিখ এবং সময় প্রাথমিকভাবে অথবা শক্তি হারানোর পর সেট করার জন্য।
