---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.329249-07:00
description: ''
lastmod: '2024-04-05T21:59:54.729277-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001Arduino IDE\u306B`RTClib`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u3044\u308B\u3053\u3068\
  \u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002\u6B21\u306B\u3001\u305D\
  \u306E\u6587\u66F8\u306B\u5F93\u3063\u3066\u3001RTC\u30E2\u30B8\u30E5\u30FC\u30EB\
  \u3092Arduino\u306B\u63A5\u7D9A\u3057\u307E\u3059."
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
