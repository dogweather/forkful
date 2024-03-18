---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:17:43.330345-06:00
description: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB\u0987 \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u0986\u09B0\u09A1\
  \u09C1\u0987\u09A8\u09CB \u0993\u09AF\u09BC\u09C7\u09AC\u09C7\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0995\u09A5\u09BE \u09AC\u09B2\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\
  \u09BC, \u09AF\u09C7\u09AE\u09A8 \u0995\u09CB\u09A8\u09CB \u09B8\u09BE\u09B0\u09CD\
  \u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u0995\u09BF\u099B\u09C1 \u09A1\u09C7\
  \u099F\u09BE \u09AB\u09C7\u09B0\u09A4 \u09AA\u09BE\u09A0\u09BE\u09A4\u09C7 \u09AC\
  \u09B2\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0986\u09B0\u09A1\u09C1\u0987\u09A8\
  \u09CB\u0995\u09C7 API-\u098F\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.319379-06:00'
model: gpt-4-0125-preview
summary: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB\u0987 \u09B9\u09B2\u09CB \u0986\u09AA\u09A8\u09BE\u09B0 \u0986\u09B0\u09A1\
  \u09C1\u0987\u09A8\u09CB \u0993\u09AF\u09BC\u09C7\u09AC\u09C7\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0995\u09A5\u09BE \u09AC\u09B2\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\
  \u09BC, \u09AF\u09C7\u09AE\u09A8 \u0995\u09CB\u09A8\u09CB \u09B8\u09BE\u09B0\u09CD\
  \u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 \u0995\u09BF\u099B\u09C1 \u09A1\u09C7\
  \u099F\u09BE \u09AB\u09C7\u09B0\u09A4 \u09AA\u09BE\u09A0\u09BE\u09A4\u09C7 \u09AC\
  \u09B2\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u09A4\u09BE\u09A6\u09C7\u09B0 \u0986\u09B0\u09A1\u09C1\u0987\u09A8\
  \u09CB\u0995\u09C7 API-\u098F\u09B0\u2026"
title: "HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3\
  \ \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

HTTP অনুরোধ পাঠানোই হলো আপনার আরডুইনো ওয়েবের সাথে কথা বলার উপায়, যেমন কোনো সার্ভার থেকে কিছু ডেটা ফেরত পাঠাতে বলা। প্রোগ্রামাররা তাদের আরডুইনোকে API-এর সাথে মিথস্ক্রিয়া করতে, ওয়েব কন্টেন্ট আহরণ করতে, অথবা অন্যান্য ইন্টারনেট-ভিত্তিক সেবাগুলোর সাথে যোগাযোগ করতে এটি করেন।

## কিভাবে:

আরডুইনোর সাথে কাজ করার জন্য `WiFiNINA` লাইব্রেরিটি নেটওয়ার্ক বৈশিষ্ট্যের জন্য প্রয়োজন। এখানে একটি সাধারণ GET অনুরোধ পাঠানোর উপায় দেওয়া হলো:

```Arduino
#include <WiFiNINA.h>

char ssid[] = "yourNetworkName";       // আপনার নেটওয়ার্কের SSID (নাম)
char pass[] = "yourNetworkPass";       // আপনার নেটওয়ার্কের পাসওয়ার্ড
int status = WL_IDLE_STATUS;           // ওয়াইফাই রেডিও'র অবস্থা
char server[] = "example.com";         // যে সার্ভারে আপনি যুক্ত হতে চান

WiFiClient client;

void setup() {
  Serial.begin(9600);                  // ডিবাগিংয়ের জন্য সিরিয়াল শুরু করা
  WiFi.begin(ssid, pass);              // ওয়াইফাই সংযোগ শুরু করা
  while (status != WL_CONNECTED) {     // সংযোগের জন্য অপেক্ষা করা:
    status = WiFi.status();
    delay(1000);
  }
  Serial.print("Connected to ");
  Serial.println(ssid);
}

void loop() {
  if (client.connect(server, 80)) {    // যদি আপনি একটি সংযোগ পান, অনুরোধ পাঠান:
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();                   // অনুরোধের শেষ
  } else {
    Serial.println("Connection failed"); // যদি আপনি সার্ভারে সংযোগ পাননি:
  }

  while (client.connected()) {         // যখন আপনি সংযুক্ত থাকেন, ডেটা পড়ুন:
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  if (!client.connected()) {           // যদি সার্ভার সংযুক্তি বিচ্ছিন্ন হয়, ক্লায়েন্টকে বন্ধ করে দিন:
    client.stop();
  }

  delay(10000);                        // আবার চেষ্টা করার আগে দশ সেকেন্ড অপেক্ষা করুন
}
```

নমুনা আউটপুট:
```
HTTP/1.1 200 OK
Date: Mon, 23 Jan 2023 12:36:47 GMT
Server: Apache/2.4.1 (Unix)
...
```

## গভীর ডুব

মাইক্রোকন্ট্রোলার থেকে HTTP অনুরোধ পাঠানোর ধারণা সবসময় ছিল না। অতীতে, মাইক্রোকন্ট্রোলারগুলো বেশি সেন্সর এবং ভৌতিক বিশ্বের সাথে ইন্টার‌্যাকশনের বিষয়ে ছিল। কিন্তু IoT (ইন্টারনেট অফ থিংস) এর উদ্ভবের সাথে, এই ডিভাইসগুলোকে ওয়েব সংযোগের দরকার হতে থাকে। এখন আরডুইনো `WiFiNINA` এর মতো লাইব্রেরিগুলি ব্যবহার করে এই সংযোগগুলি নির্ভরযোগ্যভাবে পরিচালনা করতে পারে।

`WiFiNINA` ছাড়াও অন্যান্য বিকল্পগুলি আপনার হার্ডওয়্যারের উপর নির্ভর করে। যেমন, `Ethernet` লাইব্রেরিটি তারের সংযোগকে লিভারেজ দেয়, অন্যদিকে `WiFi101` পুরনো WiFi শিল্ডের সাথে কাজ করে।

বাস্তবায়নের পাশে, HTTP অনুরোধ পাঠানোটি সরল মনে হতে পারে, কিন্তু হ্যান্ডশেক, হেডারগুলি, এবং HTTP পদ্ধতিগুলি (GET, POST, ইত্যাদি) একটি কঠোর প্রোটোকলের অংশ, যা ডিভাইসগুলিকে ওয়েবের মাধ্যমে যোগাযোগ করতে দেয়। আরডুইনো এই জটিলতার অনেক কিছু সহজ করে দেয়, তবে মৌলিক জ্ঞান রাখা যখন জিনিসগুলি ঠিকভাবে কাজ করে না তখন সমস্যা সমাধানে সাহায্য করে।

## আরো দেখুন

- Arduino `WiFiNINA` লাইব্রেরি ডকস: https://www.arduino.cc/en/Reference/WiFiNINA
- HTTP প্রোটোকল প্রাইমার: https://developer.mozilla.org/en-US/docs/Web/HTTP
- ওয়েব-সংযুক্ত প্রকল্পের জন্য Arduino প্রকল্প হাব: https://create.arduino.cc/projecthub
