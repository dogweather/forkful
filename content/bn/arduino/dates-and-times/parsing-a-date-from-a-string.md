---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:05:17.260104-06:00
description: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C1\u09AF\
  \u09BC\u09BE\u09B2 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\u09CD\
  \u09AC \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u0989\
  \u09AA\u09BE\u09A6\u09BE\u09A8\u0997\u09C1\u09B2\u09BF (\u09AC\u099B\u09B0, \u09AE\
  \u09BE\u09B8, \u09A6\u09BF\u09A8) \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\
  \u099F\u09BF \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.332031-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u09B9\u09B2 \u099F\u09C7\u0995\u09CD\u09B8\u099F\u09C1\u09AF\
  \u09BC\u09BE\u09B2 \u09AA\u09CD\u09B0\u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\u09CD\
  \u09AC \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u0989\
  \u09AA\u09BE\u09A6\u09BE\u09A8\u0997\u09C1\u09B2\u09BF (\u09AC\u099B\u09B0, \u09AE\
  \u09BE\u09B8, \u09A6\u09BF\u09A8) \u098F\u0995\u09CD\u09B8\u099F\u09CD\u09B0\u09CD\
  \u09AF\u09BE\u0995\u09CD\u099F \u0995\u09B0\u09C7 \u098F\u09AE\u09A8 \u098F\u0995\
  \u099F\u09BF \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কি এবং কেন?

আরডুইনোতে একটি স্ট্রিং থেকে তারিখ পার্স করা মানে হল টেক্সটুয়াল প্রতিনিধিত্ব থেকে তারিখের উপাদানগুলি (বছর, মাস, দিন) এক্সট্র্যাক্ট করে এমন একটি ফরম্যাটে রূপান্তর করা যা সময় রেখায়, তুলনায় বা স্কেচগুলিতে ম্যানিপুলেশনের জন্য ব্যবহৃত হতে পারে। প্রোগ্রামাররা প্রায়ই এই কাজটি করে থাকেন যাতে রিয়েল-টাইম ক্লক, লগার বা ওয়েব API এবং ব্যবহারকারীর ইন্টারফেস থেকে ইনপুট প্রক্রিয়া করা যায়, যেখানে তারিখগুলি পাঠযোগ্য ফরম্যাটে প্রদান করা হতে পারে।

## কিভাবে:

তৃতীয়-পক্ষের লাইব্রেরি ছাড়াই সরাসরি প্রক্রিয়া:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // YYYY-MM-DD ফরম্যাটে উদাহরণ তারিখ স্ট্রিং
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // পার্স করা উপাদানগুলির সাথে একটি DateTime অবজেক্ট ইনিশিয়ালাইজ করুন
  DateTime parsedDate(year, month, day);
  
  Serial.print("পার্স করা তারিখ: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

নমুনা আউটপুট:
```
পার্স করা তারিখ: 2023/4/1
```

আরডুইনোJson সহ তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করে (যখন JSON প্রতিক্রিয়া থেকে তারিখ পাওয়ার মতো জটিল পার্সিং পরিস্থিতি থাকে):

প্রথমে, আরডুইনো লাইব্রেরি ম্যানেজারের মাধ্যমে ArduinoJson লাইব্রেরি ইন্সটল করুন।

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // JSON প্রতিক্রিয়া অনুমান করা
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // তারিখ স্ট্রিং এক্সট্র্যাক্ট করা
  const char* date = doc["date"];

  // আগের মতো স্ট্রিং থেকে তারিখ পার্স করুন
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("JSON থেকে পার্স করা তারিখ: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

নমুনা আউটপুট:
```
JSON থেকে পার্স করা তারিখ: 2023/7/19
```
