---
title:                "デバッグ出力を表示する"
aliases:
- ja/arduino/printing-debug-output.md
date:                  2024-01-20T17:52:03.118530-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？
デバッグ出力は、コードが正しく動作しているかを確認するために使います。プログラマは問題を見つけ修正するためにこれを行います。

## How to:
やり方：

```Arduino
void setup() {
  // Start the serial communication
  Serial.begin(9600);
}

void loop() {
  // Print debug output to the serial monitor
  Serial.println("Sensor value: " + String(analogRead(A0)));
  // Wait for a second
  delay(1000);
}
```

サンプル出力:
```
Sensor value: 402
Sensor value: 403
Sensor value: 408
```

## Deep Dive:
深い情報：

Arduinoでは、シリアルコミュニケーションをデバッグに利用します。これは、Arduinoプラットフォームが登場して以来の伝統的な方法です。`Serial.print()` や `Serial.println()` は基本的なデバッグ手段で、シリアルポートを経由してコンピュータのシリアルモニタにデータを送信します。他の選択肢としては、LEDの点滅やLCDスクリーンへの出力がありますが、この手法の簡単さと有効性は非常に高いです。`String()` 関数で数字を文字列に変換し、読みやすい形でデータを表示することも可能です。

実装上の注意点としては、シリアル通信はArduinoボードとPC間でデータをやり取りするため、`Serial.begin()` で通信速度(ボーレート)を設定する必要があります。また、プログラムがリリース版の時は、デバッグ出力用コードを削除するのが一般的です。

## See Also:
関連情報：

- [プログラミングにおけるデバッグの技術](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
