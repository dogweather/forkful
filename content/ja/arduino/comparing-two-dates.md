---
title:                "日付を比較する"
aliases:
- ja/arduino/comparing-two-dates.md
date:                  2024-01-20T17:32:22.078474-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を比較する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/comparing-two-dates.md"
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
