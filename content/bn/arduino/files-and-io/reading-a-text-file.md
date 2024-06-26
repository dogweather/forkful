---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:08:06.301030-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AF\u09A6\u09BF \u09B8\u09AC\
  \u0995\u09BF\u099B\u09C1 \u09A0\u09BF\u0995\u09AE\u09A4\u09CB \u09B8\u0982\u09AF\
  \u09CB\u099C\u09BF\u09A4 \u098F\u09AC\u0982 \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\
  \u09BE \u09B9\u09DF, \u09A4\u09BE\u09B9\u09B2\u09C7 \u09B8\u09BF\u09B0\u09BF\u09AF\
  \u09BC\u09BE\u09B2 \u09AE\u09A8\u09BF\u099F\u09B0\u09C7 `example.txt` \u098F\u09B0\
  \ \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1 \u09A6\u09C7\u0996\
  \u09BE\u09AC\u09C7\u0964."
lastmod: '2024-04-05T21:53:52.892945-06:00'
model: gpt-4-0125-preview
summary: "\u09AF\u09A6\u09BF \u09B8\u09AC\u0995\u09BF\u099B\u09C1 \u09A0\u09BF\u0995\
  \u09AE\u09A4\u09CB \u09B8\u0982\u09AF\u09CB\u099C\u09BF\u09A4 \u098F\u09AC\u0982\
  \ \u099A\u09BE\u09B2\u09C1 \u0995\u09B0\u09BE \u09B9\u09DF, \u09A4\u09BE\u09B9\u09B2\
  \u09C7 \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2 \u09AE\u09A8\u09BF\u099F\
  \u09B0\u09C7 `example.txt` \u098F\u09B0 \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\
  \u09CD\u09A4\u09C1 \u09A6\u09C7\u0996\u09BE\u09AC\u09C7\u0964."
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
