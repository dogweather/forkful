---
date: 2024-01-20 17:46:46.825955-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.373562-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\
  \u308B\u306B\u306F`String`\u30AF\u30E9\u30B9\u306E`length()`\u30E1\u30BD\u30C3\u30C9\
  \u3092\u4F7F\u3044\u307E\u3059\u30021980\u5E74\u4EE3\u306BC\u8A00\u8A9E\u3067\u306E\
  `strlen()`\u95A2\u6570\u306E\u767B\u5834\u304B\u3089\u3001\u591A\u304F\u306E\u8A00\
  \u8A9E\u304C\u3053\u308C\u3092\u5B9F\u88C5\u3057\u3066\u304D\u307E\u3057\u305F\u3002\
  Arduino\u3067\u3082`length()`\u306F\u5358\u7D14\u660E\u5FEB\u3002\u305F\u3060\u3057\
  \u3001`String`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306F\u52D5\u7684\u30E1\u30E2\
  \u30EA\u3092\u4F7F\u7528\u3059\u308B\u305F\u3081\u3001\u30E1\u30E2\u30EA\u30D5\u30E9\
  \u30B0\u30E1\u30F3\u30C6\u30FC\u30B7\u30E7\u30F3\u306E\u30EA\u30B9\u30AF\u304C\u3042\
  \u308A\u307E\u3059\u3002\u3053\u306E\u305F\u3081\u306B\u3001`char`\u914D\u5217\u3068\
  C\u30B9\u30BF\u30A4\u30EB\u306E\u6587\u5B57\u5217\u95A2\u6570\u3092\u4F7F\u3046\u3053\
  \u3068\u3082\u3067\u304D\u307E\u3059\u3002\u305F\u3060\u3057\u3001\u4F7F\u7528\u65B9\
  \u6CD5\u306F\u5C11\u3057\u8907\u96D1\u3067\u3059\u3002"
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
