---
date: 2024-01-20 17:55:52.851001-07:00
description: "How to: (\u65B9\u6CD5) Arduino\u74B0\u5883\u3067\u306F\u30B3\u30DE\u30F3\
  \u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306F\u76F4\u63A5\u6271\u3048\u306A\u3044\u3051\
  \u3069\u3001\u30B7\u30EA\u30A2\u30EB\u901A\u4FE1\u3092\u4F7F\u3063\u3066\u4F3C\u305F\
  \u3053\u3068\u304C\u3067\u304D\u308B\u3088\u3002\u4E0B\u306F\u305D\u306E\u4F8B\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.516676-06:00'
model: gpt-4-1106-preview
summary: "Arduino\u74B0\u5883\u3067\u306F\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\
  \u5F15\u6570\u306F\u76F4\u63A5\u6271\u3048\u306A\u3044\u3051\u3069\u3001\u30B7\u30EA\
  \u30A2\u30EB\u901A\u4FE1\u3092\u4F7F\u3063\u3066\u4F3C\u305F\u3053\u3068\u304C\u3067\
  \u304D\u308B\u3088\u3002\u4E0B\u306F\u305D\u306E\u4F8B."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
