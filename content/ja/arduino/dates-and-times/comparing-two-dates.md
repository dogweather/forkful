---
date: 2024-01-20 17:32:22.078474-07:00
description: "How to: (\u65B9\u6CD5) \u4EE5\u4E0B\u306B\u3001\u4E8C\u3064\u306E\u65E5\
  \u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\u3002\
  \u6BD4\u8F03\u7D50\u679C\u3092\u898B\u308B\u305F\u3081\u306E\u30B5\u30F3\u30D7\u30EB\
  \u51FA\u529B\u3082\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.513375-06:00'
model: gpt-4-1106-preview
summary: "\u4EE5\u4E0B\u306B\u3001\u4E8C\u3064\u306E\u65E5\u4ED8\u3092\u6BD4\u8F03\
  \u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\u3002\u6BD4\u8F03\u7D50\u679C\
  \u3092\u898B\u308B\u305F\u3081\u306E\u30B5\u30F3\u30D7\u30EB\u51FA\u529B\u3082\u542B\
  \u307E\u308C\u3066\u3044\u307E\u3059."
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (方法)
以下に、二つの日付を比較する方法を示します。比較結果を見るためのサンプル出力も含まれています。

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 25, 2, 2023); // 設定する時刻 (時、分、秒、日、月、年)
  time_t now = now(); // 現在の時刻を取得

  time_t eventTime = makeTime(11, 30, 0, 5, 3, 2023); // イベント時刻を設定

  if(now > eventTime) {
    Serial.println("Event has passed.");
  } else if(now < eventTime) {
    Serial.println("Event is upcoming.");
  } else {
    Serial.println("It's event time!");
  }
}

void loop() {
  // ここでは何もしない
}

```
サンプル出力：
```
Event is upcoming.
```

## Deep Dive (探究)
日付の比較は、1970年のUNIX時代から通用している原則です。タイムスタンプとは、特定の日時からの経過秒数を数える方法です。`TimeLib.h` ライブラリは、この時代を超えた概念を Arduinoにもたらしています。代替として `RTC` (Real Time Clock) モジュールを使い、ハードウェアベースで時刻を追跡する手法もあります。実装時には、時間の同期や夏時間の調整などの詳細注意が必要です。

## See Also (関連情報)
- Arduino Time Library: https://www.arduino.cc/en/Reference/Time
- Arduino RTC Library: https://github.com/adafruit/RTClib
- Unix Time: https://en.wikipedia.org/wiki/Unix_time
