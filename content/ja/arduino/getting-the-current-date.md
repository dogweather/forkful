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

## 何 & なぜ？
現在の日付を取得するとは、プログラマーが現在の日付を取得するための方法です。プログラマーは、プログラムが正しい日付を使用していることを確認するため、または特定のタスクを実行するために現在の日付を必要としています。

## 方法：
```Arduino
// 現在の日付を取得するためのコード例
#include <ctime>

void setup(){
  // シリアルポートの設定
  Serial.begin(9600);
}

void loop(){
  // 現在の日付を取得
  time_t now = time(NULL);

  // 日付を文字列に変換してシリアルモニターに出力
  Serial.println(ctime(&now));
}

```

実行結果：
```
Fri Jul 23 16:25:42 2021
```

## 詳細を見る：
現在の日付を取得する方法は、ハードウェアやソフトウェアの進歩とともに変化してきました。昔は、時計回路やGPSモジュールを使用して日付を取得していましたが、現在ではArduinoの内蔵ライブラリを使用することで簡単に日付を取得することができます。その他の方法としては、インターネット上のサービスから日付を取得することもできます。

## 関連情報：
- [Arduino Time Library](https://playground.arduino.cc/Code/Time/)
- [Arduinoの内蔵ライブラリについて](https://www.arduino.cc/reference/en/language/functions/time/date/)
- [インターネットを使用して日付を取得する方法](https://www.instructables.com/Arduino-Real-Time-Clock-Using-Internet-Time/)