---
aliases:
- /ja/arduino/searching-and-replacing-text/
date: 2024-01-20 17:56:56.962796-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u3068\u306F\u3001\
  \u6587\u5B57\u5217\u306E\u4E2D\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u3092\u898B\u3064\u3051\u51FA\u3057\u3001\u305D\u306E\u90E8\u5206\u3092\u5225\u306E\
  \u30C6\u30AD\u30B9\u30C8\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u4FEE\
  \u6B63\u3001\u30C7\u30FC\u30BF\u306E\u6574\u7406\u3001\u307E\u305F\u306F\u81EA\u52D5\
  \u5316\u51E6\u7406\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.139422
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u3068\u306F\u3001\
  \u6587\u5B57\u5217\u306E\u4E2D\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u3092\u898B\u3064\u3051\u51FA\u3057\u3001\u305D\u306E\u90E8\u5206\u3092\u5225\u306E\
  \u30C6\u30AD\u30B9\u30C8\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u4FEE\
  \u6B63\u3001\u30C7\u30FC\u30BF\u306E\u6574\u7406\u3001\u307E\u305F\u306F\u81EA\u52D5\
  \u5316\u51E6\u7406\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

テキスト検索と置換とは、文字列の中から特定のパターンを見つけ出し、その部分を別のテキストで置き換えることです。プログラマーは、コードの修正、データの整理、または自動化処理のためにこれを行います。

## How to: (やり方)

```Arduino
String text = "Hello, world!";
String searchText = "world";
String replaceText = "Arduino";

text.replace(searchText, replaceText);
Serial.println(text); // "Hello, Arduino!"
```

## Deep Dive (深堀り)

テキストの検索と置換は文字列操作の基本で、1970年代の初期のテキストエディターから存在します。Arduinoにおける`String.replace()`メソッドはシンプルだが、大量のデータや長い文字列ではメモリ使用が問題になることがある。代替案としては、`char`配列を使う独自の関数を作ることで、サイズが大きい置換作業に対処することが可能です。 

## See Also (関連情報)

- Arduino String reference: [Arduino Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Memory Management with Arduino: [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
