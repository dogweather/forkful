---
title:    "Arduino: 将来または過去の日付の計算"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ日付計算をするのか
日付計算を行う理由はさまざまです。例えば、特定の日付を含むイベントを予定したい場合や、過去の日付を知りたい場合などがあります。Arduinoを使用することで、簡単かつ正確に日付を計算することができます。

## 方法
日付計算を行うためには、まず指定した日付からの経過日数を計算する必要があります。その後、指定した日数を加減算することで目的の日付を求めることができます。以下の例では、現在の日付から100日後の日付を計算する方法を示します。

```Arduino
#include <DateTime.h>

DateTime today = DateTime.now();
int daysToAdd = 100;

DateTime futureDate = today + TimeSpan(daysToAdd);
Serial.println("Future date: " + futureDate.toStr());
``` 
上記のコードを実行すると、`今日の日付 + 100日`後の日付がシリアルモニターに表示されます。このように、DateTimeライブラリを使用することで簡単に日付計算が行えます。

## 詳細を深く掘り下げる
日付計算を行うときには、注意点がいくつかあります。まず、DateTimeオブジェクトを作成する際には、必ず現在の日付を基準とする必要があります。また、加減算する日数の値に負の値を設定することで過去の日付を計算できます。さらに、DateTimeオブジェクトは時刻を含んでいるため、必要ない場合は`.date()`関数を使用して日付のみを取得することができます。

## 参考リンク
- ArduinoのDateTimeライブラリの公式ドキュメント: https://www.arduino.cc/reference/en/libraries/datetime/
- 日付計算の方法について詳しく解説した記事（英語）: https://www.bcs.org/upload/pdf/does-it-add-up-cedward-theory.pdf
- Arduinoで日付を扱うためのDateTimeライブラリについて分かりやすく説明したサイト（英語）: https://learn.sparkfun.com/tutorials/working-with-time/how-do-date--time-work