---
date: 2024-01-20 17:55:52.851001-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3063\u3066\u4F55\
  \uFF1F \u30D7\u30ED\u30B0\u30E9\u30E0\u8D77\u52D5\u6642\u306B\u5916\u90E8\u304B\u3089\
  \u4E0E\u3048\u308B\u8FFD\u52A0\u60C5\u5831\u3060\u3088\u3002\u4F55\u3067\u4F7F\u3046\
  \u306E\uFF1F \u81EA\u52D5\u5316\u3068\u304B\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u306E\
  \u305F\u3081\u306B\u306D\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.516676-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3063\u3066\u4F55\
  \uFF1F \u30D7\u30ED\u30B0\u30E9\u30E0\u8D77\u52D5\u6642\u306B\u5916\u90E8\u304B\u3089\
  \u4E0E\u3048\u308B\u8FFD\u52A0\u60C5\u5831\u3060\u3088\u3002\u4F55\u3067\u4F7F\u3046\
  \u306E\uFF1F \u81EA\u52D5\u5316\u3068\u304B\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u306E\
  \u305F\u3081\u306B\u306D\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数って何？ プログラム起動時に外部から与える追加情報だよ。何で使うの？ 自動化とかカスタマイズのためにね。

## How to: (方法)
Arduino環境ではコマンドライン引数は直接扱えないけど、シリアル通信を使って似たことができるよ。下はその例。

```Arduino
void setup() {
  Serial.begin(9600);  // シリアル通信の開始
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');  // 改行までの文字列を読み取り
    if (command == "LED_ON") {
      digitalWrite(LED_BUILTIN, HIGH);  // LEDを点灯
      Serial.println("LED is ON");
    } else if (command == "LED_OFF") {
      digitalWrite(LED_BUILTIN, LOW);  // LEDを消灯
      Serial.println("LED is OFF");
    } else {
      Serial.println("Unknown Command");  // 不明なコマンド
    }
  }
}
```

サンプル出力：
```
LED is ON
LED is OFF
Unknown Command
```

## Deep Dive (詳細情報)
Arduinoはマイクロコントローラーで基本的にコマンドライン引数を取り扱わない。でも、シリアル通信をつかってPCや別のデバイスから指示を受け取れる。歴史的背景？ UNIXやDOS時代からコマンドライン引数はあるよ。代替方法？ もちろん、BluetoothやWiFiを使ってデータを受け取る事も可能。実装の詳細？ Serial.readStringUntil()はタイムアウトも設定できるし、細かい挙動を調整できるよ。

## See Also (参考資料)
- Arduino公式サイトシリアル通信チュートリアル: [Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- シリアル通信の基礎知識: [Serial communication basics](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent)
- Arduino Forum: [Arduino Forum](https://forum.arduino.cc/)
