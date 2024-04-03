---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:06.301030-06:00
description: "\u0986\u09B0\u09CD\u09A6\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 \u098F\u0995\
  \u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B8\u09A1\u09BF \u0995\u09BE\
  \u09B0\u09CD\u09A1 \u0985\u09A5\u09AC\u09BE \u09A1\u09BF\u09AD\u09BE\u0987\u09B8\
  \u09C7\u09B0 \u09AE\u09C7\u09AE\u09B0\u09BF\u09A4\u09C7 \u09B8\u0982\u09B0\u0995\
  \u09CD\u09B7\u09BF\u09A4 \u09A1\u09BE\u099F\u09BE \u0986\u09A8\u09BE \u2014 \u09B8\
  \u09C7\u099F\u09BF\u0982\u09B8, \u0995\u09CD\u09AF\u09BE\u09B2\u09BF\u09AC\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09A1\u09BE\u099F\u09BE, \u0985\u09A5\u09AC\u09BE \u09B2\
  \u0997\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\
  \u099C\u09A8\u0995\u0964\u2026"
lastmod: '2024-03-17T18:47:44.340130-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09CD\u09A6\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 \u098F\u0995\
  \u099F\u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\u09B8\u09A1\u09BF \u0995\u09BE\
  \u09B0\u09CD\u09A1 \u0985\u09A5\u09AC\u09BE \u09A1\u09BF\u09AD\u09BE\u0987\u09B8\
  \u09C7\u09B0 \u09AE\u09C7\u09AE\u09B0\u09BF\u09A4\u09C7 \u09B8\u0982\u09B0\u0995\
  \u09CD\u09B7\u09BF\u09A4 \u09A1\u09BE\u099F\u09BE \u0986\u09A8\u09BE \u2014 \u09B8\
  \u09C7\u099F\u09BF\u0982\u09B8, \u0995\u09CD\u09AF\u09BE\u09B2\u09BF\u09AC\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09A1\u09BE\u099F\u09BE, \u0985\u09A5\u09AC\u09BE \u09B2\
  \u0997\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09C1\u09AC\u09BF\u09A7\u09BE\
  \u099C\u09A8\u0995\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8 \u0995\u09CB\u09A1 \u09A5\u09C7\u0995\u09C7 \u09A1\u09BE\u099F\u09BE \u09AA\
  \u09C3\u09A5\u0995 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u09AF\u09BE\
  \ \u0986\u09AA\u09A1\u09C7\u099F \u098F\u09AC\u0982 \u09AC\u09CD\u09AF\u09AC\u09B8\
  \u09CD\u09A5\u09BE\u09AA\u09A8\u09BE\u0995\u09C7 \u09B8\u09B9\u099C \u0995\u09B0\
  \u09C7 \u09A4\u09CB\u09B2\u09C7\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\
  \u09BC\u09BE"
weight: 22
---

## কিভাবে:
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // সিরিয়াল পোর্টের সংযোগের জন্য অপেক্ষা করা হচ্ছে।
  }

  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }

  myFile = SD.open("example.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening example.txt");
  }
}

void loop() {
  // setup এর পরে আর কিছু হয় না
}
```

যদি সবকিছু ঠিকমতো সংযোজিত এবং চালু করা হয়, তাহলে সিরিয়াল মনিটরে `example.txt` এর বিষয়বস্তু দেখাবে।

## গভীর ডুব
ঐতিহাসিকভাবে, আর্দুইনোর মতো মাইক্রোকন্ট্রোলারের খুব ছোট মেমরি থাকত এবং ফাইল সামলাতে পারত না। কিন্তু এসডি কার্ড মডিউল এবং বড় অনবোর্ড মেমরির সঙ্গে, আমরা এখন ফাইল I/O পাচ্ছি। এই উদ্দেশ্যে বেশ কয়েকটি লাইব্রেরি আছে, যেমন `<SD.h>`. এটি `<SPI.h>`-এর ওপর নির্মিত যা এসপিআই বাসের মাধ্যমে এসডি কার্ডের সাথে যোগাযোগ করে।

বিকল্পের কথা বলতে গেলে, আপনি ছোট ডাটার জন্য EEPROM (non-volatile memory) ব্যবহার করতে পারেন অথবা একটি আর্দুইনোকে একটি নেটওয়ার্কে সংযোজন করে একটি সার্ভার থেকে ফাইল আনতে পারেন। `<SD.h>` লাইব্রেরিটি নিম্ন-স্তরের ফাংশনগুলির জন্য একটি আবরণ, যা ফাইল ব্যবস্থাপনা, পড়া এবং লেখার কাজকে মানক C++ স্ট্রিমের মতো সহজ করে তোলে।

আর্দুইনোতে বাস্তবায়নের জন্য এসডি কার্ড মডিউল চালু করা, ফাইলটি খোলা, আর কিছু না পড়া যাওয়া পর্যন্ত তা পড়া এবং তারপর তা বন্ধ করে দেওয়া যাতে সম্পদগুলি মুক্ত হয়, প্রয়োজনীয়। ফাইল অপারেশনের মধ্যে সাধারণ মাথা ব্যথার সাধারণ কারণ হিসাবে চালু বা ফাইল খুলতে ব্যর্থ হওয়ার মতো ত্রুটিগুলি সামলানো অপরিহার্য।

## দেখুন এছাড়াও
- অফিশিয়াল SD লাইব্রেরি রেফারেন্স: https://www.arduino.cc/en/Reference/SD
- সিরিয়াল যোগাযোগের জন্য আর্দুইনোর SPI লাইব্রেরি: https://www.arduino.cc/en/reference/SPI
- ছোট ডাটা স্টোরেজ কাজের জন্য আর্দুইনোর সাথে EEPROM ব্যবহারের গাইড: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite
