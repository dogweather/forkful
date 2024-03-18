---
title:                "বেসিক অথেন্টিকেশন সহ HTTP রিকুয়েস্ট প্রেরণ"
date:                  2024-03-17T18:18:33.595877-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
মৌলিক প্রমাণীকরণ সহ HTTP অনুরোধ পাঠানো একটি নিরাপত্তা স্তর যোগ করে যা একটি ব্যবহারকারী নাম এবং পাসওয়ার্ড চায়। প্রোগ্রামাররা এটি কেবল অনুমোদিত ব্যবহারকারীদের জন্য আটকে দেওয়া APIs বা ওয়েব পরিষেবাগুলিতে প্রবেশ করার জন্য ব্যবহার করে।

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
