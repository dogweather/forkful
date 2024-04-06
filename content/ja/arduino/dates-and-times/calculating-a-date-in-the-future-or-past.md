---
date: 2024-01-20 17:31:00.408905-07:00
description: "How to: (\u65B9\u6CD5) Sample Output."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.327034-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (方法)
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, setting the time!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__))); // Set to compile time
  }
}

void loop() {
  DateTime now = rtc.now(); // Current date and time
  DateTime futureDate = now + TimeSpan(30,0,0,0); // 30 days into the future
  DateTime pastDate = now - TimeSpan(30,0,0,0); // 30 days into the past
  
  Serial.print("Current Date: ");
  printDateTime(now);
  Serial.print("Future Date: ");
  printDateTime(futureDate);
  Serial.print("Past Date: ");
  printDateTime(pastDate);
  
  delay(10000); // Wait for 10 seconds
}

void printDateTime(const DateTime &dt) {
  Serial.print(dt.year(), DEC);
  Serial.print('/');
  Serial.print(dt.month(), DEC);
  Serial.print('/');
  Serial.print(dt.day(), DEC);
  Serial.println();
}
```

Sample Output:
```
Current Date: 2023/4/5
Future Date: 2023/5/5
Past Date: 2023/3/6
```

## Deep Dive (深掘り)
RTClibはリアルタイムクロックモジュールのためのライブラリです。歴史的に、RTC (リアルタイムクロック) は時間記憶に電池を用いて永続性を確保しました。RTC_DS3231は、温度補償型の高精度RTCです。過去・未来の日付を単純に「DateTime」オブジェクトに「TimeSpan」を加算・減算する形で計算します。TimeSpanは日、時、分、秒で構成されます。Arduinoには他にもTimeライブラリなどがありますが、RTClibの方が精度が高いためよく使われます。実装には、RTCモジュールと通信するためにI2Cプロトコルを使用します。

## See Also (関連情報)
- [RTClib GitHub Repository](https://github.com/adafruit/RTClib)
- [DS3231 Datasheet](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf)
