---
date: 2024-01-20 17:46:46.825955-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3053\
  \u3068\u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u304C\u3044\u304F\u3064\u306E\u6587\
  \u5B57\u304B\u3089\u6210\u308B\u304B\u3092\u6570\u3048\u4E0A\u3052\u308B\u30D7\u30ED\
  \u30BB\u30B9\u3067\u3059\u3002\u30E1\u30E2\u30EA\u7BA1\u7406\u3001\u5165\u529B\u691C\
  \u8A3C\u3001\u307E\u305F\u306FUI\u8868\u793A\u3092\u6B63\u3057\u304F\u884C\u3046\
  \u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.483522-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3053\
  \u3068\u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u304C\u3044\u304F\u3064\u306E\u6587\
  \u5B57\u304B\u3089\u6210\u308B\u304B\u3092\u6570\u3048\u4E0A\u3052\u308B\u30D7\u30ED\
  \u30BB\u30B9\u3067\u3059\u3002\u30E1\u30E2\u30EA\u7BA1\u7406\u3001\u5165\u529B\u691C\
  \u8A3C\u3001\u307E\u305F\u306FUI\u8868\u793A\u3092\u6B63\u3057\u304F\u884C\u3046\
  \u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
