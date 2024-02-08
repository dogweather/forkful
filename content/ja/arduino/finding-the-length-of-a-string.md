---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:46:46.825955-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？何のために？)
文字列の長さを見つけることは、その文字列がいくつの文字から成るかを数え上げるプロセスです。メモリ管理、入力検証、またはUI表示を正しく行うために、プログラマーはこれを行います。

## How to: (方法)
```Arduino
void setup() {
  Serial.begin(9600);
  String greeting = "こんにちは";
  int length = greeting.length();
  Serial.println(length);
}

void loop() {
  // nothing to do here
}
```
サンプル出力:
```
5
```

## Deep Dive (深掘り)
文字列の長さを見つけるには`String`クラスの`length()`メソッドを使います。1980年代にC言語での`strlen()`関数の登場から、多くの言語がこれを実装してきました。Arduinoでも`length()`は単純明快。ただし、`String`オブジェクトは動的メモリを使用するため、メモリフラグメンテーションのリスクがあります。このために、`char`配列とCスタイルの文字列関数を使うこともできます。ただし、使用方法は少し複雑です。

## See Also (関連情報)
- ArduinoのStringリファレンス: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- メモリ管理についての情報: https://learn.arduino.cc/programming/best-practices/effective-use-of-memory
- C言語の文字列関数について: http://www.cplusplus.com/reference/cstring/
