---
title:                "文字列の連結"
html_title:           "Arduino: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
文字列の連結（concatenation）とは、複数の文字列を一つに結合することです。プログラマーは、異なる文字列を組み合わせて新しい文章やメッセージを作り出すために、この技術を使います。

## How to:
```Arduino
// 文字列の連結方法の例
String name = "太郎";
String greeting = "こんにちは";
String message = name + "さん、" + greeting + "！";
Serial.println(message);
```
Output: 太郎さん、こんにちは！

## Deep Dive:
### Historical Context:
文字列の連結は、プログラミング言語において古くから使われてきた技術です。過去には、文字列を結合するために、+演算子や書式指定子など様々な方法が用いられていました。しかし現在では、多くの言語で文字列の連結を行うための特別な機能が用意されています。

### Alternatives:
文字列の連結には、+演算子以外にも、String.concat()やString.format()などの関数があります。これらの関数を使うことで、より簡単に文字列を連結することができます。

### Implementation Details:
Arduinoでは、文字列をconcatenateするための+演算子が用意されています。この演算子は、Stringクラスのオンラインメソッドとして実装されており、異なるデータ型の値を連結することができます。ただし、文字列の連結は大量のメモリを消費するため、大規模なプログラムでは注意が必要です。

## See Also:
- [Stringクラスリファレンス](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [コラム - 文字列の連結について](https://www.arduino.cc/en/Tutorial/StringCompare)
- [C言語における文字列操作](https://ja.wikipedia.org/wiki/C%E8%A8%80%E8%AA%9E%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%96%87%E5%AD%97%E5%88%97%E6%93%8D%E4%BD%9C)