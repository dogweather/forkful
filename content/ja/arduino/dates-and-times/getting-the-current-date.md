---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.329249-07:00
description: "\u65B9\u6CD5\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.511068-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u81EA\u4F53\u306B\u306F\u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\
  \u30AF\u30ED\u30C3\u30AF\uFF08RTC\uFF09\u3092\u6B20\u3044\u3066\u3044\u308B\u305F\
  \u3081\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u76F4\u63A5\u53D6\u5F97\u3059\u308B\
  \u7D44\u307F\u8FBC\u307F\u30E1\u30BD\u30C3\u30C9\u306F\u3042\u308A\u307E\u305B\u3093\
  \u3002\u3057\u304B\u3057\u3001DS3231\u306E\u3088\u3046\u306A\u5916\u90E8RTC\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3068\u3001\u3053\u308C\u3089\u306E\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u3068\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3092\u7C21\u5358\
  \u306B\u3059\u308BAdafruit\u306B\u3088\u3063\u3066\u958B\u767A\u3055\u308C\u305F\
  `RTClib`\u306A\u3069\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u3067\u3001\u3053\u308C\u3092\u5B9F\u73FE\u3067\u304D\u307E\u3059\
  ."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
Arduino自体には、リアルタイムクロック（RTC）を欠いているため、現在の日付を直接取得する組み込みメソッドはありません。しかし、DS3231のような外部RTCモジュールと、これらのモジュールとのインターフェイスを簡単にするAdafruitによって開発された`RTClib`などのライブラリを使用することで、これを実現できます。

まず、Arduino IDEに`RTClib`ライブラリがインストールされていることを確認してください。次に、その文書に従って、RTCモジュールをArduinoに接続します。

はじめにこちらのシンプルな例をご覧ください：

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("RTCが見つかりませんでした");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTCの電力が失われました。時間を設定しましょう！");
    // 新しいデバイスに時間を設定する必要がある場合や電力を失った後に、ここで設定できます。
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("現在の日付: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // シリアルスパムを減らすために3秒間遅延
}
```

サンプル出力（RTCが以前に設定されていると仮定）：

```
現在の日付: 2023/4/15
```

このコードはRTCモジュールを初期化し、ループ内で、毎3秒ごとに現在の日付をシリアルモニターに出力し続けます。`rtc.adjust(...)`の行は、RTCの日付と時間を初めて、または電力を失った後に設定するためにアンコメントして修正することができます。
