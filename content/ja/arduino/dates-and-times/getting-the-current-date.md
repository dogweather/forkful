---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.329249-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.511068-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3067\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u30ED\u30B0\u8A18\u9332\
  \u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u306E\u8A2D\u5B9A\u3001\u307E\u305F\
  \u306F\u30BF\u30B9\u30AF\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u306B\
  \u4E0D\u53EF\u6B20\u306A\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u60C5\u5831\u3092\u53D6\
  \u5F97\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u6A5F\u80FD\u6027\u3092\u5411\u4E0A\u3055\u305B\u3001\
  \u30C7\u30FC\u30BF\u306E\u95A2\u9023\u6027\u3092\u78BA\u4FDD\u3057\u3001IoT\u304A\
  \u3088\u3073\u7D44\u307F\u8FBC\u307F\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3067\u6642\
  \u9593\u306B\u654F\u611F\u306A\u64CD\u4F5C\u3092\u4FC3\u9032\u3059\u308B\u305F\u3081\
  \u306B\u3001\u3053\u306E\u6A5F\u80FD\u304C\u3088\u304F\u5FC5\u8981\u3068\u3055\u308C\
  \u307E\u3059\u3002."
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
