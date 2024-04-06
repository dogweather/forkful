---
date: 2024-01-20 17:56:56.962796-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.294359-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\
  \u63DB\u306F\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u57FA\u672C\u3067\u30011970\u5E74\
  \u4EE3\u306E\u521D\u671F\u306E\u30C6\u30AD\u30B9\u30C8\u30A8\u30C7\u30A3\u30BF\u30FC\
  \u304B\u3089\u5B58\u5728\u3057\u307E\u3059\u3002Arduino\u306B\u304A\u3051\u308B\
  `String.replace()`\u30E1\u30BD\u30C3\u30C9\u306F\u30B7\u30F3\u30D7\u30EB\u3060\u304C\
  \u3001\u5927\u91CF\u306E\u30C7\u30FC\u30BF\u3084\u9577\u3044\u6587\u5B57\u5217\u3067\
  \u306F\u30E1\u30E2\u30EA\u4F7F\u7528\u304C\u554F\u984C\u306B\u306A\u308B\u3053\u3068\
  \u304C\u3042\u308B\u3002\u4EE3\u66FF\u6848\u3068\u3057\u3066\u306F\u3001`char`\u914D\
  \u5217\u3092\u4F7F\u3046\u72EC\u81EA\u306E\u95A2\u6570\u3092\u4F5C\u308B\u3053\u3068\
  \u3067\u3001\u30B5\u30A4\u30BA\u304C\u5927\u304D\u3044\u7F6E\u63DB\u4F5C\u696D\u306B\
  \u5BFE\u51E6\u3059\u308B\u3053\u3068\u304C\u53EF\u80FD\u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
