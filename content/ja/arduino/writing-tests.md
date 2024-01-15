---
title:                "「テストの書き方」"
html_title:           "Arduino: 「テストの書き方」"
simple_title:         "「テストの書き方」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか？

あなたはArduinoを使って何かを作ろうと思ったことがありますか？もしもあなたがプロジェクトを通して理想的な動きを達成したいと思っているなら、テストを書くことは非常に重要です。テストは、コードの正しさを確認し、バグを見つけるのに役立ちます。そうすることで、プロジェクトの動きが期待通りになるように保証することができます。

## テストの書き方

まず、テストのための新しいディレクトリを作成します。そこに、すべてのテストを含む「test.ino」ファイルを作成します。テストのコードは、実際のコードに依存していないことが重要です。そのため、別々の「test.(機能名).ino」ファイルを作成することをお勧めします。例えば、もしスイッチの機能をテストしたい場合、ファイル名を「test.switch.ino」とします。

```Arduino
// ユニットテストの例

include <ArduinoUnit.h> // テスト用のライブラリ

testCase(SwitchTest) {
  int pin = 2; // スイッチのピン番号
  pinMode(pin, INPUT); // pinを設定
  assertEqual(digitalRead(pin), LOW); // ピンの状態がLOWになることを確認
}

// 出力例
✅ TEST PASSED: SwitchTest
```

## 詳しくテストの書き方を学ぶ

もしもテストの書き方についてより詳しい情報を学びたい場合は、[ArduinoUnit公式サイト](https://github.com/mmurdoch/arduinounit)を参考にしてください。そこにはより多くのテストの例があり、ArduinoUnitについてより深く学ぶことができます。

## 他にも参考になる情報

- [Arduinoの公式ドキュメント](https://www.arduino.cc/en/Guide/Introduction)
- [Arduinoのチュートリアル](https://www.arduino.cc/en/Tutorial/HomePage)
- [Arduinoユーザーフォーラム](https://forum.arduino.cc/)