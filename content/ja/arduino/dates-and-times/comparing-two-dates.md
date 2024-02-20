---
date: 2024-01-20 17:32:22.078474-07:00
description: "\u6BD4\u8F03\u3059\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u3068\u306F\u3001\
  \u30AB\u30EC\u30F3\u30C0\u30FC\u4E0A\u306E\u7570\u306A\u308B\u65E5\u3005\u3092\u6307\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\
  \u884C\u3044\u3001\u30A4\u30D9\u30F3\u30C8\u9806\u5E8F\u3001\u671F\u9650\u306E\u7BA1\
  \u7406\u3084\u7D4C\u904E\u6642\u9593\u3092\u76E3\u8996\u3059\u308B\u305F\u3081\u3067\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.628444
model: gpt-4-1106-preview
summary: "\u6BD4\u8F03\u3059\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u3068\u306F\u3001\
  \u30AB\u30EC\u30F3\u30C0\u30FC\u4E0A\u306E\u7570\u306A\u308B\u65E5\u3005\u3092\u6307\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\
  \u884C\u3044\u3001\u30A4\u30D9\u30F3\u30C8\u9806\u5E8F\u3001\u671F\u9650\u306E\u7BA1\
  \u7406\u3084\u7D4C\u904E\u6642\u9593\u3092\u76E3\u8996\u3059\u308B\u305F\u3081\u3067\
  \u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
比較する二つの日付とは、カレンダー上の異なる日々を指します。プログラマーはこれを行い、イベント順序、期限の管理や経過時間を監視するためです。

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
