---
title:                "Arduino: テストの書き方"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングをする際に、テストを書くことは非常に重要です。テストを書くことで、自分のコードが意図通りに動作するかどうかを確認することができます。また、将来的にコードを変更したり、修正したりする際にも、テストがあることで安心して変更することができます。これにより、バグを防ぐことができます。

## テストの書き方

テストを書くには、Arduinoのテストライブラリを使用します。テストライブラリを使うと、簡単にテストコードを書き始めることができます。以下のコードを参考にしてください。

```Arduino
#include <ArduinoUnit.h> // テストライブラリをインポート

// テスト対象の関数
int addNumbers(int a, int b) {
  return a + b;
}

// テストケース
test(addNumbers) {
  int result = addNumbers(3, 5);
  assertEqual(result, 8); // 結果が予想通りの値かどうかをチェック
}

void setup() {
  // シリアルモニターを初期化
  Serial.begin(9600);
  // テストを実行
  Test::run();
}

void loop() {
  // 何もしない
}
```

上記のコードでは、テストライブラリをインポートし、テスト対象の関数を定義しました。その後、テストケースを作成し、テストライブラリの`assertEqual`関数を使用して、結果が予想通りの値かどうかをチェックしています。最後に、`Test::run()`関数を呼び出し、テストを実行しています。

## テストの詳細

テストを書く際には、いくつかのポイントに気をつける必要があります。

- テストケースは、関数ごとに作成する。
- `assertEqual`関数を使用して、結果が予想通りの値かどうかをチェックする。
- テストケースの名称は、関数名と同じにする。
- `Test::run()`関数を呼び出すことで、テストを実行する。

これらのポイントを守ることで、テストコードを効率的かつ正確に書くことができます。

## 関連リンク

- [Arduinoのテストライブラリ - 公式ドキュメント](https://www.arduino.cc/reference/en/libraries/arduinounit/)
- [Arduinoユニットテストの例 - Hackster.io](https://www.hackster.io/eduinozero/arduino-unit-testing-examples-db657d)
- [TDDとは - Wikipedia](https://ja.wikipedia.org/wiki/%E6%8E%A8%E6%93%8E%E9%A7%86%E5%8B%95%E9%96%8B%E7%99%BA)