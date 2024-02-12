---
title:                "テストの作成"
aliases: - /ja/arduino/writing-tests.md
date:                  2024-02-03T19:29:43.084399-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Arduino環境でテストを書くとは、Arduinoデバイス上のコードの機能を自動的に検証するテストを作成するプロセスを指します。プログラマーは、コードが期待通りに動作することを確認し、バグを減らし、特にデバッグがより困難である組み込みシステムでプロジェクトの品質を向上させるためにこれを行います。

## 方法：

Arduinoには他のプログラミング環境のような組み込みテストフレームワークはありません。しかし、`AUnit`などのサードパーティライブラリを使用してArduinoコードの単体テストを行うことができます。AUnitはArduinoの組み込みライブラリ`ArduinoUnit`とGoogleのテストフレームワーク`Google Test`に触発されています。

### AUnitを使用した例：

まず、Arduino IDEのライブラリマネージャーからAUnitをインストールします：スケッチ > ライブラリをインクルード > ライブラリを管理... > AUnitを検索してインストールします。

次に、以下のようにテストを書きます：

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // 空
}
```
このテストをArduinoボードにアップロードした後、シリアルモニターを開いてテスト結果を表示します。各テストが合格したか失敗したかを示す出力を見ることができます：

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

このシンプルな例は、LEDピンの状態をテストするためにAUnitを使用する方法を示しています。テストを作成することで、異なる条件でのArduinoの振舞いが期待どおりであることを確認できます。AUnitを使えば、より複雑なテスト、テストスイートを書いたり、テストタイムアウトやより高度なシナリオのためのセットアップ/ティアダウン手順などの機能を楽しむことができます。
