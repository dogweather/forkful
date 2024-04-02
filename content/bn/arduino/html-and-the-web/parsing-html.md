---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:21.437006-06:00
description: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u0995\u09B2\
  \u09CD\u09AA\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\
  \u099C \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\u09AF \u0989\u09A4\u09CD\u09A4\
  \u09CB\u09B2\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB\
  \ \u09A1\u09BF\u09AD\u09BE\u0987\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u0985\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u2026"
lastmod: '2024-03-17T18:47:44.320391-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB \u09AA\u09CD\u09B0\u0995\u09B2\
  \u09CD\u09AA\u09C7 HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC \u09AA\u09C7\
  \u099C \u09A5\u09C7\u0995\u09C7 \u09A4\u09A5\u09CD\u09AF \u0989\u09A4\u09CD\u09A4\
  \u09CB\u09B2\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7\
  \u09A8 \u09A4\u09BE\u09A6\u09C7\u09B0 \u0986\u09B0\u09A1\u09C1\u0987\u09A8\u09CB\
  \ \u09A1\u09BF\u09AD\u09BE\u0987\u09B8\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BE\u09B0\u09A8\u09C7\u099F\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u0985\u09CD\u09AF\u09BE\u0995\u09CD\u099F\
  \u2026"
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কি এবং কেন?

আরডুইনো প্রকল্পে HTML পার্সিং মানে হচ্ছে ওয়েব পেজ থেকে তথ্য উত্তোলন করা। প্রোগ্রামাররা এটি করেন তাদের আরডুইনো ডিভাইসগুলিকে ইন্টারনেটের সাথে ইন্টারঅ্যাক্ট করার অনুমতি দিতে, যা বাড়ি অটোমেশন থেকে পরিবেশ নিরীক্ষণ পর্যন্ত বিভিন্ন উদ্দেশ্যে ওয়েবসাইট থেকে ডেটা সংগ্রহ করে।

## কিভাবে:

আরডুইনোতে HTML পার্সিং সাধারণত সীমিত ডিভাইস সোর্স বিবেচনায় ন্যূনতম ফুটপ্রিন্ট লাইব্রেরিগুলি দাবি করে। ওয়েব স্ক্র্যাপিং এবং পার্সিং এর জন্য একটি জনপ্রিয় পছন্দ হল `ESP8266HTTPClient` এবং `ESP8266WiFi` লাইব্রেরিগুলি ব্যবহার করা ESP8266, অথবা তাদের ESP32 সমতুল্যরা, দেওয়া Wi-Fi ক্ষমতা এবং HTTP প্রোটোকলের জন্য নেটিভ সাপোর্টের কারণে। এখানে একটি প্রাথমিক উদাহরণ রয়েছে HTML ফেচ এবং পার্স করার জন্য, ধরে নিচ্ছি আপনি ESP8266 অথবা ESP32 এর সাথে কাজ করছেন:

প্রথমে, প্রয়োজনীয় লাইব্রেরিগুলি সংযোজন করুন:
```cpp
#include <ESP8266WiFi.h> // ESP8266 এর জন্য
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// ESP32 ব্যবহার করলে সমতুল্য ESP32 লাইব্রেরিগুলি ব্যবহার করুন

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

আপনার Wi-Fi নেটওয়ার্কে সংযোগ করুন:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Connecting...");
    }
}
```

HTTP অনুরোধ করুন এবং একটি সাধারণ HTML টুকরা পার্স করুন:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //WiFi সংযোগের স্থিতি পরীক্ষা করুন
        HTTPClient http;  //HTTPClient ক্লাসের একটি অবজেক্ট ঘোষণা করুন

        http.begin("http://example.com");  //অনুরোধের গন্তব্য নির্দিষ্ট করুন
        int httpCode = http.GET();  //অনুরোধ পাঠান

        if (httpCode > 0) { //ফেরত কোড চেক করুন
            String payload = http.getString();   //অনুরোধের প্রতিউত্তরের payload পান
            Serial.println(payload);             //প্রতিউত্তরের payload প্রিন্ট করুন

            // একটি নির্দিষ্ট অংশ পার্স করুন, যেমন, payload থেকে শিরোনাম বের করা
            int titleStart = payload.indexOf("<title>") + 7; // "<title>" ট্যাগ পার হতে +7 
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("পৃষ্ঠার শিরোনাম: ");
            Serial.println(pageTitle);
        }

        http.end();   //সংযোগ বন্ধ করুন
    }

    delay(10000); //প্রতি ১০ সেকেন্ড অনুরোধ করুন
}
```

নমুনা আউটপুট (ধরে নিচ্ছি http://example.com এর HTML গঠন সাধারণ):
```
Connecting...
...
পৃষ্ঠার শিরোনাম: Example Domain
```

এই উদাহরণটি HTML পৃষ্ঠা ফেচ করা এবং `<title>` ট্যাগের বিষয়বস্তু নির্যাস করা দেখায়। আরও জটিল HTML পার্সিংয়ের জন্য, স্মরণে সীমাবদ্ধতার কারণে সাবধানতার সাথে নিয়মিত অভিব্যক্তি (regular expressions) বা স্ট্রিং ম্যানিপুলেশন ফাংশনগুলি ব্যবহার করার বিবেচনা করুন। আরও উন্নত পার্সিং জটিল পদ্ধতির দাবি করতে পারে, যা আপনার নিপূণ HTML গঠনের জন্য অভিযোজিত বৈশিষ্ট্যযুক্ত কাস্টম পার্সিং অ্যালগরিদমসহ। স্ট্যান্ডার্ড আরডুইনো পরিবেশে কোনো অন্তর্নির্মিত HTML পার্সিং লাইব্রেরি অন্তর্ভুক্ত নয়।
