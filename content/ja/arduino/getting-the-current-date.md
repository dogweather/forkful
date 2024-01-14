---
title:                "Arduino: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

Arduinoプログラミングにおいて、現在の日付を取得する必要があることがあります。例えば、特定の日付をトリガーにしてある動作をするようにプログラムする場合や、データログを取る際に日付と時間を記録する必要がある場合などが挙げられます。今回は、Arduinoで現在の日付を取得する方法について解説します。

## 方法

Arduinoで現在の日付を取得するには、最も簡単な方法はRTCモジュールを使用することです。RTCモジュールは、リアルタイムクロックと呼ばれる電子部品で、日付と時間を正確に保持することができます。

まずは、RTCモジュールをArduinoボードに接続し、ライブラリをインストールします。次に、以下のようなコードをArduino IDEに入力してください。

```Arduino
#include <RTClib.h>

RTC_DS1307 rtc; // RTCモジュールを使用するためのオブジェクトを作成

void setup() {
  Serial.begin(9600); // シリアル通信を開始
  if (! rtc.begin()) { // RTCモジュールが接続されているか確認
   Serial.println("Couldn't find RTC");
   while (1);
  }
  if (! rtc.isrunning()) { // モジュールの時刻が表示されていない場合は設定する
    Serial.println("RTC is not running!");
    // 以下の行のコメントを外し、車輪の「年月日」を修正することで設定が可能
    //rtc.adjust(DateTime(YYYY, MM, DD, hh, mm, ss));
  }
}

void loop() {
  DateTime now = rtc.now(); // 現在の日付と時間を取得
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(' ');
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  delay(1000); // 1秒毎に繰り返される
}
```

コードをアップロードし、シリアルモニタを開くと、現在の日付と時間が表示されるはずです。

## 深堀り

RTCモジュールは、バッテリーでバックアップされているため、電源が切れても日付と時間を保持することができます。また、RTCモジュールを使用することで温度センサーや加速度センサーなどのデータも取得することができます。さらに、NTPサーバーからインターネット経由で正確な日付と時間を取得することも可能です。

## 参考

- [RTCモジュールの使い方・接続方法](https://www.switch-science.com/catalog/849/)
- [RTCモジュールとは？](https://qiita.com/rimlabs/items/336c89bcae5513bb1622)
- [NTPについて](https://ja.wikipedia.org/wiki/Network_Time_Protocol)