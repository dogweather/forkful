---
title:                "現在の日付を取得する"
html_title:           "Arduino: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜこれをするのか

現在の日付を取得する理由はいくつかあります。例えば、制御や監視システムを作成する場合、日付の情報が必要になります。また、デバイスの日付設定を自動化するためにも必要です。それでは、現在の日付を取得する方法を見ていきましょう。

## 方法

```Arduino
#include <RTClib.h> 

RTC_DS1307 rtc; 

void setup() { 
  // RTCを初期化 
  rtc.begin(); 
} 

void loop() { 
  // 現在の日付を取得 
  DateTime now = rtc.now(); 

  // 日付のフォーマットを設定 
  String date = String(now.year()) + "/" + String(now.month()) + "/" + String(now.day()); 
  
  // シリアルモニターに出力 
  Serial.println(date); 
} 
```

このコードを実行すると、RTCモジュールから現在の日付が取得され、シリアルモニターに表示されます。各部分を細かく見ていきましょう。

まず、`#include <RTClib.h>`でRTCモジュールを使用するためのライブラリをインポートします。次に、RTCオブジェクトを作成し、`begin()`メソッドで初期化します。

`loop()`関数では、`now`という名前の変数にRTCから取得した現在の日付を代入します。`RTCnow()`メソッドは現在の日時を表す`DateTime`オブジェクトを返すため、`year()`や`month()`、`day()`メソッドを使って年月日を取得します。

そして、`String`クラスを使用して日付を文字列に変換し、`println()`メソッドでシリアルモニターに出力します。日付のフォーマットは自由に変更できます。

## ディープダイブ

RTCモジュールはリアルタイムクロック（RTC）チップを搭載したデバイスで、年月日や時刻を取得することができます。Arduinoでは、内蔵のRTCモジュールや外付けのモジュールを使用することができます。自分のプロジェクトに合った適切なモジュールを選択しましょう。

また、RTCモジュールを使用するには、追加のライブラリをダウンロードする必要があります。`RTClib`は最も一般的なRTCモジュール用のライブラリですが、使用するモジュールに合わせて適切なライブラリを選択しましょう。

## See Also

- [Arduinoでの日付の取得方法](https://www.arduino.cc/en/tutorial/time)
- [RTCモジュールの種類と選び方](https://qiita.com/zakzak000/items/5ffe323c2adaec108465)