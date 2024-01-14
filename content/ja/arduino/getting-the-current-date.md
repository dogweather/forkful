---
title:    "Arduino: 「現在の日付の取得」"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ

日付と時刻の情報をコーディングに組み込むことは、多くのArduinoプロジェクトで重要です。例えば、温度や湿度のセンサーを使用して、特定の日時に応じて異なるアクションを起動させることができます。日付を取得することで、より精度の高いプログラムを作成することができます。

## 使い方

日付を取得するには、`RTClib`ライブラリを使用します。まず最初に、ライブラリをインストールする必要があります。Arduino IDEを開き、メニューバーから「スケッチ」を選択し、「ライブラリをインクルード」をクリックします。その後、「ライブラリを管理」を選択し、検索バーに「RTClib」と入力します。表示されたライブラリを選択して「インストール」をクリックします。

次に、`RTClib`を使用するためのコードを書きます。下のコードを参考にしてください。

```Arduino
#include <Wire.h> // 配線用
#include "RTClib.h" // RTClibライブラリを使用する

RTC_DS1307 rtc; // RTC_DS1307をオブジェクトとして定義

void setup() {
  rtc.begin(); // RTCを初期化
  if (!rtc.isrunning()) { // RTCが正しく動作していない場合
    Serial.println("RTC is NOT running!"); // シリアルモニターにメッセージを表示
  }
}

void loop() {
  DateTime now = rtc.now(); // 現在の日付と時刻を取得
  Serial.print("Today is: "); // 「Today is:」というメッセージを表示
  Serial.print(now.month(), DEC); // メッセージに現在の月を表示
  Serial.print('/'); // 「/」を表示
  Serial.print(now.day(), DEC); // メッセージに現在の日を表示
  Serial.print('/'); // 「/」を表示
  Serial.println(now.year(), DEC); // メッセージに現在の年を表示

  Serial.print("Current time is: "); // 「Current time is:」というメッセージを表示
  Serial.print(now.hour(), DEC); // メッセージに現在の時を表示
  Serial.print(':'); // 「:」を表示
  Serial.print(now.minute(), DEC); // メッセージに現在の分を表示
  Serial.print(':'); // 「:」を表示
  Serial.print(now.second(), DEC); // メッセージに現在の秒を表示
  Serial.println(" (Japan Time)"); // メッセージに「 (Japan Time)」という文言を表示

  delay(1000); // 1秒待機
}
```

上記のコードは、RTCを初期化し、現在の日付と時刻を取得し、シリアルモニターに表示するものです。必要に応じて、`now`オブジェクトからさまざまな情報を抽出して使用することができます。

## ディープダイブ

`RTClib`では、さまざまなメソッドを使用して日付と時刻の情報を取得することができます。例えば、`year()`、`month()`、`day()`は、それぞれ現在の年、月、日を返します。また、`hour()`、`minute()`、`second()`はそれぞれ現在の時、分、秒を返します。さらに、`weekday()`は現在の曜日を返します。

また、RTCの時刻を設定することもできます。`adjust()`メソッ