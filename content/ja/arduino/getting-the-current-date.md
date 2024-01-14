---
title:                "Arduino: 「現在の日付を取得する」"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
あなたはプログラミングやロボット工学に興味がありますか？それともスマートホームを作りたいですか？どのような目的であっても、時刻や日付を取得することはとても重要です。アルドゥイーノを使って現在の日付を取得する方法を紹介します。

## 使い方
```Arduino 
#include <RTClib.h>

RTC_DS1307 rtc; //RTCモジュールの初期化

void setup() {
  Serial.begin(9600); //シリアルモニターの初期化
  rtc.begin(); //RTCモジュールの初期化
}

void loop() {
  DateTime now = rtc.now(); //現在の日付を取得
  Serial.print(now.year()); //年を表示
  Serial.print("-");
  Serial.print(now.month()); //月を表示
  Serial.print("-");
  Serial.println(now.day()); //日を表示
  
  delay(1000); //1秒待機
}
```

このコードを実行すると、シリアルモニターに現在の日付が表示されます。

```
2021-10-20
```

月や日の表示形式は、必要に応じて修正することができます。

## 詳細を掘り下げる
アルドゥイーノでは、RTCモジュールを使用することで現在の日付を取得することができます。RTCモジュールは、リアルタイムクロックと呼ばれるデバイスで、日付や時刻を保持することができます。アルドゥイーノとRTCモジュールを組み合わせることで、電源を切っても時刻や日付がリセットされないようにすることができます。

また、RTCモジュールは通常、I2C通信プロトコルを使用してアルドゥイーノと連携します。I2C通信は、デジタル信号を使ってデバイス間でデータをやりとりすることができる方法です。アルドゥイーノにはI2C通信用のピンがあり、RTCモジュールとの接続に使用することができます。

## 参考リンク
- [RTCモジュールを使ってアルドゥイーノで時刻を取得する方法](https://www.arduino.cc/en/Tutorial/DS1307RealTimeClock)
- [I2C通信の仕組みと使用方法](https://www.arduino.cc/en/Tutorial/TwoPortReceive)