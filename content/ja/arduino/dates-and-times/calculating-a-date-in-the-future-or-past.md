---
date: 2024-01-20 17:31:00.408905-07:00
description: "How to: (\u65B9\u6CD5) RTClib\u306F\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\
  \u30AF\u30ED\u30C3\u30AF\u30E2\u30B8\u30E5\u30FC\u30EB\u306E\u305F\u3081\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3067\u3059\u3002\u6B74\u53F2\u7684\u306B\u3001RTC (\u30EA\
  \u30A2\u30EB\u30BF\u30A4\u30E0\u30AF\u30ED\u30C3\u30AF)\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.402167-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) RTClib\u306F\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u30AF\u30ED\
  \u30C3\u30AF\u30E2\u30B8\u30E5\u30FC\u30EB\u306E\u305F\u3081\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3067\u3059\u3002\u6B74\u53F2\u7684\u306B\u3001RTC (\u30EA\u30A2\u30EB\
  \u30BF\u30A4\u30E0\u30AF\u30ED\u30C3\u30AF) \u306F\u6642\u9593\u8A18\u61B6\u306B\
  \u96FB\u6C60\u3092\u7528\u3044\u3066\u6C38\u7D9A\u6027\u3092\u78BA\u4FDD\u3057\u307E\
  \u3057\u305F\u3002RTC_DS3231\u306F\u3001\u6E29\u5EA6\u88DC\u511F\u578B\u306E\u9AD8\
  \u7CBE\u5EA6RTC\u3067\u3059\u3002\u904E\u53BB\u30FB\u672A\u6765\u306E\u65E5\u4ED8\
  \u3092\u5358\u7D14\u306B\u300CDateTime\u300D\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306B\u300CTimeSpan\u300D\u3092\u52A0\u7B97\u30FB\u6E1B\u7B97\u3059\u308B\u5F62\
  \u3067\u8A08\u7B97\u3057\u307E\u3059\u3002TimeSpan\u306F\u65E5\u3001\u6642\u3001\
  \u5206\u3001\u79D2\u3067\u69CB\u6210\u3055\u308C\u307E\u3059\u3002Arduino\u306B\u306F\
  \u4ED6\u306B\u3082Time\u30E9\u30A4\u30D6\u30E9\u30EA\u306A\u3069\u304C\u3042\u308A\
  \u307E\u3059\u304C\u3001RTClib\u306E\u65B9\u304C\u7CBE\u5EA6\u304C\u9AD8\u3044\u305F\
  \u3081\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002\u5B9F\u88C5\u306B\u306F\u3001\
  RTC\u30E2\u30B8\u30E5\u30FC\u30EB\u3068\u901A\u4FE1\u3059\u308B\u305F\u3081\u306B\
  I2C\u30D7\u30ED\u30C8\u30B3\u30EB\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
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
