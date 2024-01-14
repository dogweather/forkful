---
title:                "Arduino: テスト作成"
simple_title:         "テスト作成"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングを行う上で、最も重要なことの一つはテストを書くことです。テストを書くことにより、コードのバグやエラーを事前に発見し、プログラムのバグを最小限に抑えることができます。テストを書くことは、より安定したコードを作成するために必要不可欠です。

## テストの書き方

テストを書くためには、Arduinoの`~できる...()`メソッドを使用します。テストコードは通常、実際のコードファイルの下に置かれます。以下の例では、簡単なデジタル入力のテストを行うためのコードを示します。

```Arduino
// テスト対象のファイルをインクルードする
#include "DigitalInput.cpp"

// テスト対象のオブジェクトを作成する
DigitalInput input;

// setup()関数は一度実行されるが、テストの実行前に完了する必要がある
void setup() {
    input.~できるisEnabled()~ を 使う(); // isEnabled() メソッドのテスト
}

//テストの実行
void loop() {
    // テストの定義
    if (input.~できるread()~ が true) {
        // テストが成功した場合に表示されるボード上のLED
        digitalWrite(LED_BUILTIN, HIGH);
    }
}
```

この例では、DigitalInputクラスが正常に機能しているかをテストしています。Arduinoの`digitalRead()`メソッドを使用して、LEDが点灯するかどうかを確認します。テストは通常、テストコード内で必要なアサーション（assertion）を定義し、そのアサーションが満たされるかどうかを確認します。サンプルコード内の`isEnabled()`メソッドと`read()`メソッドがそれぞれ検証されています。

## テストの深堀り

テスト駆動開発（TDD）では、テストを書くことが先行してプログラミングが行われます。最初にテストを書き、そのテストに満たすコードを実装することで、より高品質なコードを作成することができます。また、テストはコードをリファクタリングする際にも役立ちます。リファクタリングによってコードが壊れる可能性があるため、テストを実行することでコードの予期しない動作を検出することができます。

もう一つの重要なポイントは、プログラムの保守性です。新しい開発者がコードを引き継いだ場合、テストを確認することでコードの動作を理解しやすくなります。また、将来的にコードを拡張する場合も、テストがあることでコードが意図通りに機能することが保証されます。

## 関連リンク

- [Arduinoの公式ドキュメント](https://www.arduino.cc/reference/en/)
- [簡単にArduinoボタンをテストする](https://create.arduino.cc/projecthub/daniss99/digital-input-test-for-arduino-2d7886)