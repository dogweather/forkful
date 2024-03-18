---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:36.873904-06:00
description: "\u0986\u09B0\u09CD\u09A1\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 CSV (Comma-Separated\
  \ Values) \u09AB\u09BE\u0987\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B8\u09A1\u09BF \u0995\u09BE\
  \u09B0\u09CD\u09A1\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u09B8\
  \u09BF\u098F\u09B8\u09AD\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7\
  \ \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09B8\u09BF\u098F\u09B8\u09AD\u09BF\
  \ \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE, \u09AF\u09BE \u09A1\u09C7\
  \u099F\u09BE \u09B2\u0997\u09BF\u0982, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\
  \u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.345297-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09CD\u09A1\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 CSV (Comma-Separated\
  \ Values) \u09AB\u09BE\u0987\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B8\u09A1\u09BF \u0995\u09BE\
  \u09B0\u09CD\u09A1\u09C7 \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09BF\u09A4 \u09B8\
  \u09BF\u098F\u09B8\u09AD\u09BF \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7\
  \ \u09AA\u09A1\u09BC\u09BE \u098F\u09AC\u0982 \u09B8\u09BF\u098F\u09B8\u09AD\u09BF\
  \ \u09AB\u09BE\u0987\u09B2\u09C7 \u09B2\u09C7\u0996\u09BE, \u09AF\u09BE \u09A1\u09C7\
  \u099F\u09BE \u09B2\u0997\u09BF\u0982, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\
  \u09C7\u09B6\u09A8\u2026"
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
আর্ডুইনোতে CSV (Comma-Separated Values) ফাইল নিয়ে কাজ করা মানে এসডি কার্ডে সংরক্ষিত সিএসভি ফাইল থেকে পড়া এবং সিএসভি ফাইলে লেখা, যা ডেটা লগিং, কনফিগারেশন সেটিংস, এবং আরও অনেক কিছুকে সম্ভব করে। প্রোগ্রামাররা প্রায়শই সারল্য এবং বিভিন্ন প্ল্যাটফর্ম জুড়ে ব্যাপক গ্রহণযোগ্যতার কারণে সেন্সর ডেটা সংগ্রহ, কনফিগারেশন প্যারামিটার স্টোরেজ, অথবা অন্যান্য সিস্টেমের সাথে ইন্টারফেসিংয়ের জন্য সিএসভি নিয়ে কাজ করে থাকে।

## কিভাবে:
আর্ডুইনোতে CSV ফাইল হ্যান্ডল করার জন্য কোনো বিল্ট-ইন লাইব্রেরি নেই, তবে আপনি এসডি কার্ডে ফাইল অ্যাক্সেসের জন্য `SD` এবং `SPI` লাইব্রেরিগুলি ব্যবহার করতে পারেন, এবং তারপর মৌলিক স্ট্রিং ম্যানিপুলেশন কৌশল ব্যবহার করে CSV ডেটা পার্স করা বা জেনারেট করা যেতে পারে। যখন আরও জটিল CSV ম্যানিপুলেশনের সাথে মোকাবিলা করা লাগে, তৃতীয়-পক্ষের লাইব্রেরি `ArduinoCSV` সহজে পার্সিং এবং লেখার জন্য ব্যবহৃত হতে পারে।

**এসডি কার্ড থেকে CSV ডেটা পড়া:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // CSV লাইন প্রিন্ট করে
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // এই উদাহরণে ব্যবহৃত নয়
}
```
*নমুনা আউটপুট:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**এসডি কার্ডে CSV ডেটা লেখা:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // CSV হেডার
    dataFile.println("1, 1597840923, 23.5"); // উদাহরণের ডেটা সারি
    dataFile.close();
    Serial.println("Data written");
  } else {
    Serial.println("Error opening output.csv");
  }
}

void loop() {
  // এই উদাহরণে ব্যবহৃত নয়
}
```
*নমুনা আউটপুট:*
```
Data written
```

**পার্সিংয়ের জন্য ArduinoCSV ব্যবহার করা:**
জটিল CSV ফাইলের সাথে মোকাবিলা করার সময়, `ArduinoCSV` লাইব্রেরি পার্সিং প্রচেষ্টা অনেক সহজ করে দিতে পারে। এই উদাহরণটি ধরে নেয় যে আপনি ইতিমধ্যে `ArduinoCSV` লাইব্রেরি ইনস্টল করেছেন।

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // প্রতিটি ক্ষেত্র প্রিন্ট করুন
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // এই উদাহরণে ব্যবহৃত নয়
}
```
*নমুনা আউটপুট:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
এই উদাহরণগুলোতে, এসডি কার্ডের উপর CSV ফাইল থেকে পড়া এবং লেখার মাধ্যমে, আর্ডুইনো প্রকল্পগুলি সহজেই ডেটা সংগ্রহ করতে, কনফিগারেশন সেটিংস সংরক্ষণ করতে, অথবা অন্যান্য অ্যাপ্লিকেশনের সাথে ডেটা বিনিময় করতে পারে একটি সর্বজনীনভাবে অ্যাক্সেসযোগ্য ফরম্যাটে।
