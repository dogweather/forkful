---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:33.595877-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u099F\u09BF \u098F\u0995\
  \u099F\u09BF \u0985\u09CD\u09AF\u09BE\u09B0\u09CD\u09A1\u09C1\u0987\u09A8\u09CB\u09A4\
  \u09C7 \u0998\u099F\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\
  \u09A8\u09BE\u0995\u09C7 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7 \u09AA\u09CD\u09B0\
  \u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF \u09B8\u0982\u09AF\u09C1\u0995\u09CD\u09A4\
  \u09BF \u0995\u09B0\u09BE \u09A6\u09B0\u0995\u09BE\u09B0 - \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3\u09A4 ESP8266 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF `<ESP8266WiFi.h>`\
  \ \u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.322409-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u099F\u09BF \u098F\u0995\u099F\u09BF \u0985\u09CD\u09AF\u09BE\u09B0\
  \u09CD\u09A1\u09C1\u0987\u09A8\u09CB\u09A4\u09C7 \u0998\u099F\u09BE\u09A8\u09CB\u09B0\
  \ \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09AA\u09CD\u09B0\
  \u09A5\u09AE\u09C7 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\
  \ \u09B8\u0982\u09AF\u09C1\u0995\u09CD\u09A4\u09BF \u0995\u09B0\u09BE \u09A6\u09B0\
  \u0995\u09BE\u09B0 - \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 ESP8266 \u098F\u09B0\
  \ \u099C\u09A8\u09CD\u09AF `<ESP8266WiFi.h>` \u09AC\u09BE ESP32 \u098F\u09B0 \u099C\
  \u09A8\u09CD\u09AF `<WiFi.h>`, \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09AE\u09BE\
  \u09A3\u09C0\u0995\u09B0\u09A3 \u09AC\u09BF\u09AC\u09B0\u09A3 \u098F\u09A8\u0995\
  \u09CB\u09A1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `<Base64.h>`\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u0996\u09BE\u09B2\u09BF\
  -\u0996\u09BE\u09B2\u09BF \u09B8\u09CD\u09A8\u09BF\u09AA\u09C7\u099F \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2 \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
এটি একটি অ্যার্ডুইনোতে ঘটানোর জন্য, আপনাকে প্রথমে প্রয়োজনীয় লাইব্রেরিগুলি সংযুক্তি করা দরকার - সাধারণত ESP8266 এর জন্য `<ESP8266WiFi.h>` বা ESP32 এর জন্য `<WiFi.h>`, এবং প্রমাণীকরণ বিবরণ এনকোড করার জন্য `<Base64.h>`। এখানে একটি খালি-খালি স্নিপেট দেওয়া হল আপনাকে শুরু করার জন্য:

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "yourSSID";
const char* password = "yourPassword";
const char* server = "your.server.com";
const char* authUser = "user";
const char* authPass = "pass";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  String auth = "Basic " + base64::encode(String(authUser) + ":" + String(authPass));

  WiFiClient client;
  if (client.connect(server, 80)) {
    client.println("GET /route HTTP/1.1");
    client.print("Host: ");
    client.println(server);
    client.println("Authorization: " + auth);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // আপনার নিয়মিত কোড এখানে
}
```

চালানোর সময়, অ্যার্ডুইনো নির্দিষ্ট সার্ভারে প্রমাণীকরণগুলি সহ সংযুক্ত হবে এবং সুরক্ষিত কন্টেন্ট আনবে।

## গভীরে যাওয়া
HTTP মৌলিক প্রমাণীকরণ 1996 সালে RFC 2617 দ্বারা সংজ্ঞায়িত ওয়েবের প্রাথমিক দিনগুলিতে ছিল। এটি সাধারণ: ব্যবহারকারী নাম এবং পাসওয়ার্ডকে base64 এ এনকোড করা এবং একটি HTTP হেডারে যোগ করা। এটি সবচেয়ে নিরাপদ পদ্ধতি নয় (কারণ base64 সহজেই উলটানো যেতে পারে), কিন্তু কম ঝুঁকি বা অভ্যন্তরীণ টুলের জন্য এটি সরল।

আছে বিকল্প, যেমন ডাইজেস্ট অ্যাক্সেস প্রমাণীকরণ বা OAuth, যা আরও নিরাপদ, কিন্তু তারা রিসোর্সের উপর ভারী - একটি ছোট অ্যার্ডুইনোতে বিবেচনা করার বিষয়।

বাস্তবায়নের জন্য, মনে রাখবেন যে base64 এনকোডিং cred-এর আকার প্রায় 33% বাড়িয়ে দেয়, এবং অ্যার্ডুইনোর মেমরি সীমিত। সাথে সাথে, যদি আপনি ইন্টারনেটের মাধ্যমে creds পাঠান তবে SSL/TLS (HTTPS) ব্যবহার করে আপনার সার্ভার নিশ্চিত করুন এড়ানোর জন্য।

## দেখুন সেও
- [মৌলিক অ্যাক্সেস প্রমাণীকরণে উইকিপিডিয়া](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [আপনার HTTP অনুরোধ নিরাপদ রাখুন](https://arduino.cc/en/Tutorial/WebClientRepeating)
