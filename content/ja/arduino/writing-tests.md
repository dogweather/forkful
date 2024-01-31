---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

category:             "Arduino"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
プログラムテストとは、エラーを見つけて修正するためのコードを書くことです。品質を保つため、また、将来的な不具合を未然に防ぐためにプログラマーが行います。

## How to: (方法)
Arduinoコードにおけるテストは、通常、シリアルモニタで結果を確認する形で行います。以下はLEDの点滅をテストするサンプルコードです。

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);   // LEDを点灯
  delay(1000);                       // 1秒待つ
  digitalWrite(LED_BUILTIN, LOW);    // LEDを消灯
  delay(1000);                       // 1秒待つ
}

```

サンプル出力:
```
LEDが1秒ごとに点滅します。
```

## Deep Dive (深掘り)
Arduinoのテストは、通常、物理的なハードウェアの動作を手動で確認することが多いですが、ソフトウェアのみのテストも可能です。過去には、シミュレーション環境や特定のライブラリを使ってソフトウェアの挙動をテストする方法が取られていました。代替手段としてMockオブジェクトを用いる方法や、連続的インテグレーション（CI）ツールを使用する手法などがあります。実装の詳細として、セットアップ関数やループ関数に特定のテストコードを書いて、シリアルモニタで結果を観察します。

## See Also (関連情報)
- Arduinoの公式ドキュメント: https://www.arduino.cc/reference/en/
- オンラインArduinoシミュレータ: https://www.tinkercad.com/circuits
- テストを学ぶためのArduinoプロジェクト: https://create.arduino.cc/projecthub/projects/tags/test
- ArduinoでのCIツールの利用: https://learn.adafruit.com/continuous-integration-arduino-and-you/overview
