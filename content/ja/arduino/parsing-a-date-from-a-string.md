---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:34:22.181067-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から日付を解析する: 文字列の中にある日付情報を読み取る作業です。データ記録やイベント管理など、プログラムが日付を理解し扱う必要があるために行います。

## How to: (やり方)
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (!rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
  }
  
  // 以下の行で日付をセット。年、月、日、時、分、秒の順
  rtc.adjust(DateTime(2023, 1, 21, 3, 0, 0));
}

void loop() {
  DateTime now = rtc.now();
  
  // 日付を YYYY/MM/DD 形式で出力
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  
  delay(1000);
}
```
出力例: `2023/1/21`

## Deep Dive (深掘り)
日付の解析が必要になるのは、古くはコンピュータが人間の読める形式の日付を理解しなければならない場面で始まりました。Arduino で日付文字列を扱うには、RTCライブラリ（実時間クロック）を使用し、`DateTime`クラスを活用します。`DateTime`オブジェクトは年、月、日、時、分、秒を保持し、これらを個別に読み出したり、特定のフォーマットで出力することができます。

代わりに、文字列パーサを自作することも可能ですが、RTCライブラリを使う方が信頼性が高く再利用しやすいです。パースの際の注意点としては、月と日は1から始まること、年は4桁であることを確認しましょう。

## See Also (関連情報)
- Arduino RTC Library: https://www.arduino.cc/reference/en/libraries/rtclib/
- Arduino `DateTime` class: https://www.arduino.cc/en/Reference/DateTime
- Date and Time functions: https://www.arduino.cc/reference/en/language/functions/time/
