---
title:    "Arduino: テストを書く"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## なぜArduinoプログラミングにテストを導入するか

Arduinoを使用すると、実際に物理的なものを制御することができますが、プログラムのバグは大きな問題を引き起こす可能性があります。そこで、テストを使用してプログラムの信頼性を確保することが重要です。

## テストの書き方

Arduinoでは、ArduinoUnitと呼ばれるライブラリを使用してテストを書くことができます。以下のようにテストを開始することができます。

```Arduino
#include <ArduinoUnit.h>

// テストするコード
void setup() {
  // 初期化のコード
}

// テストするコード
void loop() {
  // 実際のコード
}

// ここからテストを開始する
test(functionName) {
  // 期待する結果を書く
  // 実際の結果を比較する
  assertEquals(expected, actual);
}
```

テストを実行するには、シリアルモニターで"Test"と入力してEnterキーを押すだけです。

## テストの深堀り

ArduinoUnitでは、テストケースやテストスイートなど、様々な機能を使用することができます。また、デバイスやセンサーをシミュレーションするためのライブラリもあります。どのようにテストを構成するかは、プロジェクトの規模や目的によって異なりますが、テストの信頼性を高めるためにも、様々な機能を活用することをお勧めします。

## 参考リンク

[ArduinoUnit公式ドキュメント](https://github.com/mmurdoch/arduinounit)

[Arduinoを使ったテスト駆動開発（TDD）についてのブログポスト](https://www.sigongtech.com/blog/2013/05/28/test-driven-development-for-arduino/)

[TDDを活用したArduinoプロジェクトの例](https://www.hackster.io/tddgreensboro/test-driven-development-for-arduino-7be3a7)