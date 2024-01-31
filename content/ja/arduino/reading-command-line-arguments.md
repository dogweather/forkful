---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:55:52.851001-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-command-line-arguments.md"
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
