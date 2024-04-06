---
date: 2024-01-20 17:35:56.705642-07:00
description: "How to: (\u65B9\u6CD5) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B\uFF1A`2023/04/01\
  \ 12:34:56`."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.325113-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B\uFF1A`2023/04/01 12:34:56`."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

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
