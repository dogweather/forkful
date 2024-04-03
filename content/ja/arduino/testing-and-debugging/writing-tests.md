---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:43.084399-07:00
description: "Arduino\u74B0\u5883\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\
  \u3001Arduino\u30C7\u30D0\u30A4\u30B9\u4E0A\u306E\u30B3\u30FC\u30C9\u306E\u6A5F\u80FD\
  \u3092\u81EA\u52D5\u7684\u306B\u691C\u8A3C\u3059\u308B\u30C6\u30B9\u30C8\u3092\u4F5C\
  \u6210\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\
  \u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3001\u30D0\
  \u30B0\u3092\u6E1B\u3089\u3057\u3001\u7279\u306B\u30C7\u30D0\u30C3\u30B0\u304C\u3088\
  \u308A\u56F0\u96E3\u3067\u3042\u308B\u7D44\u307F\u8FBC\u307F\u30B7\u30B9\u30C6\u30E0\
  \u3067\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u54C1\u8CEA\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.501877-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u74B0\u5883\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\
  \u3001Arduino\u30C7\u30D0\u30A4\u30B9\u4E0A\u306E\u30B3\u30FC\u30C9\u306E\u6A5F\u80FD\
  \u3092\u81EA\u52D5\u7684\u306B\u691C\u8A3C\u3059\u308B\u30C6\u30B9\u30C8\u3092\u4F5C\
  \u6210\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\
  \u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3001\u30D0\
  \u30B0\u3092\u6E1B\u3089\u3057\u3001\u7279\u306B\u30C7\u30D0\u30C3\u30B0\u304C\u3088\
  \u308A\u56F0\u96E3\u3067\u3042\u308B\u7D44\u307F\u8FBC\u307F\u30B7\u30B9\u30C6\u30E0\
  \u3067\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u54C1\u8CEA\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
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
