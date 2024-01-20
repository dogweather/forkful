---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを見つけるとは、文字列に含まれる文字数を示すことです。この操作は、メモリ管理や文字操作などのため、プログラマーには欠かせません。

## やり方:
Arduinoでの文字列の長さを計測するためには`length()`関数を使います。以下はその使用例です:

```Arduino
String str = "Arduinoは楽しい";
Serial.println(str.length());
```
出力される値は「10」となり（全角文字は2とカウントされるため）、これが文字列の長さです。

## 深掘り
元々、文字列の長さを計測する概念は、早い時代のプログラミングでメモリ管理とバッファオーバーフローの防止に重要だったものです。さらに、製造者が文字列操作を容易にするためにC言語で`strlen()`関数が導入され、Arduinoの`length()`関数もこれを基に作られています。

同様の仕事をするアルタナティブとして`char`型配列を用いる手法があります。この場合、`strlen()`関数を使用して文字列の長さを計算します。しかしArduinoでは、`String`クラスを使用した方がより簡単で効率的とされています。

## 参考
1. [Arduino - String object and length method](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. [C++ String length and size()](https://www.w3schools.com/cpp/cpp_strings_length.asp)
3. [C++ String length function](https://www.arduino.cc/reference/tr/language/functions/communication/serial/available/)

この情報を効果的に活用し、より効率的なArduinoプログラミングを楽しんでください。