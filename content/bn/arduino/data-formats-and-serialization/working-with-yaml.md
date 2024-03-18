---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:36:58.105903-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

YAML (YAML Ain't Markup Language) একটি মানব-পাঠযোগ্য ডেটা সিরিয়ালাইজেশন মানদণ্ড, যা কনফিগারেশন ফাইল, ইন্টার-প্রোগ্রাম যোগাযোগ, এবং ডেটা সংরক্ষণের জন্য ব্যবহৃত হতে পারে। প্রোগ্রামাররা তাদের অ্যাপ্লিকেশনের কনফিগারেশন প্রক্রিয়াকে সহজ করতে, কোডে গভীরভাবে ডুবে না গিয়ে প্যারামিটার পরিবর্তন করতে, পাঠযোগ্যতা বৃদ্ধি করতে, এবং কনফিগারেশন শেয়ারিংকে আরও সহজ করতে Arduino প্রকল্পে YAML ব্যবহার করে থাকেন।

## কিভাবে করবেন:

Arduino-তে সরাসরি YAML নিয়ে কাজ করা উচ্চ-স্তরের প্রোগ্রামিং পরিবেশের মতো সোজা নয়, মেমোরি সীমাবদ্ধতা এবং নেটিভ YAML প্রক্রিয়াকরণ লাইব্রেরির অনুপস্থিতির কারণে। যাইহোক, YAML পার্সিং বা জেনারেশন প্রয়োজনীয় প্রকল্পের জন্য, একটি সাধারণ পদ্ধতি হল একটি সঙ্গী কম্পিউটার (যেমন একটি Raspberry Pi) ব্যবহার করা বা YAML ফাইলগুলিকে আরও Arduino-বান্ধব ফরম্যাটে (যেমন JSON) রূপান্তর করা, বাহ্যিক স্ক্রিপ্ট ব্যবহার করে। ডেমোস্ট্রেশনের উদ্দেশ্যে, আসুন ArduinoJson নামে একটি জনপ্রিয় লাইব্রেরি ব্যবহার করে পরবর্তী পদ্ধতি অবলম্বন করি।

**প্রথম ধাপ:** আপনার YAML কনফিগারেশনকে JSON-এ রূপান্তর করুন। আপনি অনলাইন টুল বা কমান্ড-লাইন ইউটিলিটি যেমন `yq` ব্যবহার করতে পারেন।

YAML ফাইল (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

JSON-এ রূপান্তরিত (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**দ্বিতীয় ধাপ:** ArduinoJson লাইব্রেরি ব্যবহার করে আপনার Arduino স্কেচে JSON ফাইল পার্স করুন। প্রথমে, Arduino IDE-র লাইব্রেরি ম্যানেজার মাধ্যমে ArduinoJson লাইব্রেরি ইনস্টল করা প্রয়োজন।

**তৃতীয় ধাপ:** আপনার কোডে JSON লোড এবং পার্স করুন। Arduino-র স্টোরেজ সীমাবদ্ধতার কারণে, কল্পনা করুন JSON স্ট্রিংটি একটি চলকে সংরক্ষিত বা একটি SD কার্ড থেকে পড়া হয়েছে।

মূল Arduino স্কেচ:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // এই উদাহরণের জন্য এখানে কিছু নেই
}
```

স্কেচ চালানোর পরে আউটপুট:
```
SSID: YourSSID
Password: YourPassword
```

JSON-এ পরিবর্তন করা এবং ArduinoJson লাইব্রেরি ব্যবহার করার এই পদ্ধতিটি, Arduino প্রকল্পের মধ্যে মানজালভাবে YAML কনফিগারেশন পরিচালনা করতে অনুমতি দেয়, মাইক্রোকন্ট্রোলারে সরাসরি YAML পার্সিং-এর চ্যালেঞ্জ দূরীভূত করে।
