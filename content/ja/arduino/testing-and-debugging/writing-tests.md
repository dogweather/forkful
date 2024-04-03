---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:43.084399-07:00
description: "\u65B9\u6CD5\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.501877-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u306B\u306F\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u74B0\u5883\u306E\u3088\u3046\u306A\u7D44\u307F\u8FBC\u307F\u30C6\u30B9\u30C8\u30D5\
  \u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3057\
  \u304B\u3057\u3001`AUnit`\u306A\u3069\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066Arduino\u30B3\u30FC\u30C9\
  \u306E\u5358\u4F53\u30C6\u30B9\u30C8\u3092\u884C\u3046\u3053\u3068\u304C\u3067\u304D\
  \u307E\u3059\u3002AUnit\u306FArduino\u306E\u7D44\u307F\u8FBC\u307F\u30E9\u30A4\u30D6\
  \u30E9\u30EA`ArduinoUnit`\u3068Google\u306E\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\
  \u30EF\u30FC\u30AF`Google Test`\u306B\u89E6\u767A\u3055\u308C\u3066\u3044\u307E\u3059\
  ."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
