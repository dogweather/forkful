---
title:                "現在の日付の取得"
aliases: - /ja/arduino/getting-the-current-date.md
date:                  2024-02-03T19:09:00.329249-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Arduinoプロジェクトで現在の日付を取得することは、ログ記録、タイムスタンプの設定、またはタスクのスケジューリングに不可欠なリアルタイム情報を取得することを含みます。プログラマーは、機能性を向上させ、データの関連性を確保し、IoTおよび組み込みプロジェクトで時間に敏感な操作を促進するために、この機能がよく必要とされます。

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
