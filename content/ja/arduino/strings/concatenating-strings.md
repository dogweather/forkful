---
date: 2024-01-20 17:33:57.554254-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u3001\u8907\u6570\u306E\u6587\
  \u5B57\u5217\u3092\u304F\u3063\u3064\u3051\u30661\u3064\u306B\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u305F\u308A\u3001\u30E6\
  \u30FC\u30B6\u30FC\u306B\u308F\u304B\u308A\u3084\u3059\u3044\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3092\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.450358-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u306F\u3001\u8907\u6570\u306E\u6587\
  \u5B57\u5217\u3092\u304F\u3063\u3064\u3051\u30661\u3064\u306B\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u305F\u308A\u3001\u30E6\
  \u30FC\u30B6\u30FC\u306B\u308F\u304B\u308A\u3084\u3059\u3044\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3092\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結は、複数の文字列をくっつけて1つにすることです。データを整理したり、ユーザーにわかりやすいメッセージを表示するために使います。

## How to: (やり方)
```Arduino
String str1 = "こん";
String str2 = "にちは、";
String str3 = "Arduino!";
String combined = str1 + str2 + str3; // 文字列を連結

Serial.begin(9600);
Serial.println(combined); // 結果を出力: こんにちは、Arduino!
```

## Deep Dive (深い掘り下げ)
昔、Arduinoではメモリが少なかったため、文字列の連結には注意が必要でした。代替手段として`char`配列と関数`strcat()`が使われてきましたが、扱いにくいです。現在のArduinoでは`String`クラスを使用することで、簡単に文字列の連結ができます。ただし、背後でメモリの動的確保が行われるため、使いすぎるとメモリ断片化の問題が起こる可能性がまだあります。

## See Also (関連情報)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
