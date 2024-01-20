---
title:                "未来または過去の日付の計算"
html_title:           "Arduino: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ? (What & Why?)
未来または過去の日付の計算とは、指定した日数を追加/減算することを指します。プログラマーはスケジューリングや予約システムなどのアプリケーションを作成する際にこれを行います。

## 実践 (How to)
```Arduino 
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(8, 29, 00, 1, 1, 2030); // 2030年1月1日の8:29
}

void loop() {
  time_t t = now();
  tmElements_t tm;
  breakTime(t, tm);
  Serial.print("現在の日付と時間: ");
  printDateTime(tm);

  addDays(&tm, 10); // 10日後
  Serial.print("10日後の日付と時間: ");
  printDateTime(tm);

  delay(1000);
}

void addDays(tmElements_t * tm, int days) {
  time_t t = makeTime(*tm);
  t += days * SECS_PER_DAY;
  breakTime(t, tm);
}

void printDateTime(const tmElements_t & tm) {
  Serial.print(tm.Year+1970);
  Serial.print("/");
  Serial.print(tm.Month);
  Serial.print("/");
  Serial.print(tm.Day);
  Serial.print(" ");
  Serial.print(tm.Hour);
  Serial.print(":");
  Serial.println(tm.Minute);
}
```

## 深掘り (Deep Dive)
日付計算はプログラミングの長い歴史の一部で、複雑なタイムゾーン問題やリープ年を考慮しなきゃならないため非常に点要です。今回紹介したのはArduino専用の`TimeLib`ライブラリを用いた方法ですが、より複雑なタイムゾーンの制御や精度を求める場合は`RTClib`のような他のライブラリを試すことも可能です。

## 関連リンク (See Also)
- [TimeLib公式ドキュメンテーション](https://www.arduinolibraries.info/libraries/time)
- [日付と時間を扱うRTClibライブラリ](https://www.arduino.cc/reference/en/libraries/rtclib-by-neironv)
- [Arduinoでの時間管理について](https://tutorial-arduino.com/Arduino_time_control)