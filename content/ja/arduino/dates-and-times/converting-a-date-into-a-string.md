---
date: 2024-01-20 17:35:56.705642-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u306E\
  \u306F\u30C7\u30FC\u30BF\u3092\u8868\u793A\u53EF\u80FD\u306A\u5F62\u5F0F\u306B\u3059\
  \u308B\u3053\u3068\u3002\u3053\u308C\u306B\u3088\u308A\u3001LCD\u3084\u30B7\u30EA\
  \u30A2\u30EB\u30E2\u30CB\u30BF\u3067\u8AAD\u3081\u308B\u3088\u3046\u306B\u306A\u308B\
  \u3057\u3001\u30ED\u30B0\u306B\u8A18\u9332\u3057\u3084\u3059\u304F\u306A\u308B\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:16.060071-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u306E\
  \u306F\u30C7\u30FC\u30BF\u3092\u8868\u793A\u53EF\u80FD\u306A\u5F62\u5F0F\u306B\u3059\
  \u308B\u3053\u3068\u3002\u3053\u308C\u306B\u3088\u308A\u3001LCD\u3084\u30B7\u30EA\
  \u30A2\u30EB\u30E2\u30CB\u30BF\u3067\u8AAD\u3081\u308B\u3088\u3046\u306B\u306A\u308B\
  \u3057\u3001\u30ED\u30B0\u306B\u8A18\u9332\u3057\u3084\u3059\u304F\u306A\u308B\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するのはデータを表示可能な形式にすること。これにより、LCDやシリアルモニタで読めるようになるし、ログに記録しやすくなる。

## How to: (方法)
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (! rtc.begin()) {
    Serial.println("RTC not found!");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, setting the time!");
    // Set the date and time here
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  char dateStr[20];
  snprintf(dateStr, sizeof(dateStr), "%04d/%02d/%02d %02d:%02d:%02d",
           now.year(), now.month(), now.day(),
           now.hour(), now.minute(), now.second());
  Serial.println(dateStr);
  delay(1000);
}
```
サンプル出力：`2023/04/01 12:34:56`

## Deep Dive (深い潜水)
日付を文字列にすることは、エレクトロニクスの初期から存在している。選択肢は幾つかある。`strftime`のような標準関数もあるけど、Arduinoでは`snprintf`がメモリ使用量を抑えられるので便利。実装の詳細は、`DateTime`ライブラリが内部でどのように現在の日付を管理・提供しているかに依存する。

## See Also (関連情報)
- Arduinoの公式文書: https://www.arduino.cc/reference/en/
- RTClibライブラリ: https://github.com/adafruit/RTClib
- strftime関数についての詳細: http://www.cplusplus.com/reference/ctime/strftime/
