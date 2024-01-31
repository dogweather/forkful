---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:13:14.260637-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

category:             "Arduino"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
マイクロコントローラで現在の日付を取得するって、時刻やエポックタイムに基づいて、それが必要な時に実行するイベントを知ることです。データのタイムスタンプ、時間に基づくトリガー、あるいは単にユーザーに日付を示すために使います。

## How to: (方法)
Arduinoで日付を取るには、通常RTC（リアルタイムクロック）モジュールか、インターネット経由で時刻データを取得する必要があります。

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
    Serial.println("RTC lost power, let's set the time!");
    // 以下の行で現在の日付と時刻をセットします
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  
  delay(1000); // 1秒ごとに日付と時刻を更新する
}
```

サンプル出力:

```
2023/3/15 12:45:30
2023/3/15 12:45:31
...

```

## Deep Dive (深い潜水)
RTCモジュールは、電源が落ちても日付と時刻情報を維持できる小型のバッテリーで動作します。DS3231は一般的なRTCモジュールで、高精度を誇ります。NTP（ネットワークタイムプロトコル）を使ってインターネット経由で時刻を同期する方法もありますが、インターネット接続と追加のライブラリが必要です。RTCの方がシンプルで信頼性が高い選択肢です。

## See Also (関連情報)
- RTClibライブラリのドキュメント: https://github.com/adafruit/RTClib
- NTPクライアントライブラリ: https://github.com/arduino-libraries/NTPClient
- Arduinoの時刻と時区の管理: https://www.arduino.cc/reference/en/libraries/eztime/
