---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:28:50.836878-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
