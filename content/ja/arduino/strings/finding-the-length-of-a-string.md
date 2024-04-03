---
date: 2024-01-20 17:46:46.825955-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.483522-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
