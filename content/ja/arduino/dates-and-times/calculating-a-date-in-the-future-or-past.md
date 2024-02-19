---
aliases:
- /ja/arduino/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:31:00.408905-07:00
description: "\u904E\u53BB\u307E\u305F\u306F\u5C06\u6765\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u6307\u5B9A\u3057\u305F\u65E5\u6570\u3092\u73FE\
  \u5728\u304B\u3089\u52A0\u3048\u308B\u304B\u5F15\u304F\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u65E5\u4ED8\
  \u3092\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u3057\u305F\u308A\u3001\u671F\u9650\u3092\
  \u7BA1\u7406\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.164978
model: gpt-4-1106-preview
summary: "\u904E\u53BB\u307E\u305F\u306F\u5C06\u6765\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3068\u306F\u3001\u6307\u5B9A\u3057\u305F\u65E5\u6570\u3092\u73FE\
  \u5728\u304B\u3089\u52A0\u3048\u308B\u304B\u5F15\u304F\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u65E5\u4ED8\
  \u3092\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u3057\u305F\u308A\u3001\u671F\u9650\u3092\
  \u7BA1\u7406\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
過去または将来の日付を計算するとは、指定した日数を現在から加えるか引くことです。プログラマは、イベントの日付をスケジュールしたり、期限を管理したりするためにこれを行います。

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
