---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:50.836878-06:00
description: "JSON, \u09AC\u09BE JavaScript Object Notation, \u09B9\u09B2 \u098F\u0995\
  \u099F\u09BF \u09B9\u09BE\u09B2\u0995\u09BE \u0993\u099C\u09A8\u09C7\u09B0 \u09A1\
  \u09C7\u099F\u09BE \u09AC\u09BF\u09A8\u09BF\u09AE\u09AF\u09BC \u09AB\u09B0\u09CD\
  \u09AE\u09CD\u09AF\u09BE\u099F, \u09AF\u09BE \u098F\u099F\u09BF\u0995\u09C7 Arduino\
  \ \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C \u09AC\u09BE\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:44.344279-06:00'
model: gpt-4-0125-preview
summary: "JSON, \u09AC\u09BE JavaScript Object Notation, \u09B9\u09B2 \u098F\u0995\
  \u099F\u09BF \u09B9\u09BE\u09B2\u0995\u09BE \u0993\u099C\u09A8\u09C7\u09B0 \u09A1\
  \u09C7\u099F\u09BE \u09AC\u09BF\u09A8\u09BF\u09AE\u09AF\u09BC \u09AB\u09B0\u09CD\
  \u09AE\u09CD\u09AF\u09BE\u099F, \u09AF\u09BE \u098F\u099F\u09BF\u0995\u09C7 Arduino\
  \ \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\u0997\u09C1\u09B2\u09BF\u09A4\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CB\u09B0\u09C7\u099C \u09AC\u09BE\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\
  \u09B2\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u0989\u09AA\u09AF\u09C1\u0995\u09CD\
  \u09A4 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09AC\u09BF\u09AD\u09BF\u09A8\
  \u09CD\u09A8 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\
  \ \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\u09C7, \u09AF\u09C7\u09AE\u09A8 Arduino,\
  \ \u09B8\u09B9 \u0993\u09AF\u09BC\u09C7\u09AC APIs \u09AC\u09BE \u0985\u09A8\u09CD\
  \u09AF\u09BE\u09A8\u09CD\u09AF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09B8\u09B9\u099C\u09C7 \u09A1\u09C7\u099F\u09BE \u09AC\
  \u09BF\u09A8\u09BF\u09AE\u09AF\u09BC \u09B8\u09AE\u09CD\u09AD\u09AC \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u09B0 \u09B8\u09BE\u09A6\u09BE\u09B8\
  \u09BF\u09A6\u09C7 \u098F\u09AC\u0982 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\
  \u09AF\u09A4\u09BE \u0995\u09BE\u09B0\u09A3\u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কি এবং কেন?

JSON, বা JavaScript Object Notation, হল একটি হালকা ওজনের ডেটা বিনিময় ফর্ম্যাট, যা এটিকে Arduino প্রকল্পগুলিতে ডেটা স্টোরেজ বা কনফিগারেশন ফাইলের জন্য উপযুক্ত করে তোলে। প্রোগ্রামাররা বিভিন্ন প্রোগ্রামিং পরিবেশে, যেমন Arduino, সহ ওয়েব APIs বা অন্যান্য সিস্টেমের সাথে সহজে ডেটা বিনিময় সম্ভব করার জন্য এর সাদাসিদে এবং পাঠযোগ্যতা কারণে ব্যবহার করে থাকে।

## কিভাবে:

Arduino তে JSON নিয়ে কাজ করার জন্য, `ArduinoJson` লাইব্রেরি একটি জনপ্রিয় পছন্দ কারণ এটি ব্যবহারে সহজ এবং কার্যকর। এটি JSON স্ট্রিং পার্স করা, তাদের পরিবর্তন করা, এবং অবজেক্টগুলিকে আবার JSON স্ট্রিংএ সিরিয়ালাইজ করার অনুমতি দেয়। এটি কি ভাবে ব্যবহার করবেন দেখানো হল:

1. **ArduinoJson লাইব্রেরি ইনস্টল করুন**: Arduino IDE তে Library Manager ব্যবহার করে "ArduinoJson" ইনস্টল করুন।

2. **একটি JSON স্ট্রিং ডিসেরিয়ালাইজ করুন**: এখানে দেখানো হলো কি ভাবে একটি JSON স্ট্রিং পার্স করে মান বের করতে হয়।

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // JSON ডকুমেন্ট অনুসারে আকার সামঞ্জস্য করুন
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() ব্যর্থ হয়েছে: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // খালি লুপ
}
```

নমুনা আউটপুট:

```
gps
1351824120
48.756080
2.302038
```

3. **একটি JSON স্ট্রিং এ সেরিয়ালাইজ করুন**: এখানে দেখানো হলো কি ভাবে ডেটা থেকে একটি JSON স্ট্রিং তৈরি করতে হয়।

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // ডেটা অনুসারে আকার সামঞ্জস্য করুন
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // খালি লুপ
}
```

নমুনা আউটপুট (পঠনযোগ্যতার জন্য ফর্ম্যাট করা):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

`ArduinoJson` লাইব্রেরি সফলভাবে ব্যবহার করে Arduino প্রকল্পগুলিকে মানব-পাঠযোগ্য ফরম্যাটে জটিল ডেটা কাঠামো যোগাযোগের সুযোগ দেয়, ওয়েব সেবাগুলির সাথে উন্নয়ন এবং ইন্টিগ্রেশন সুবিধাজনক করে তোলে।
