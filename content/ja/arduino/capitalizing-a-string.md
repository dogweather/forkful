---
title:                "文字列を大文字にする"
html_title:           "Arduino: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Arduinoでの文字列の大文字化

## 何となぜ？
大文字化とは、文字列の中の全ての小文字を大文字に変換する処理です。プログラマーはこれを使用して、ユーザーが入力した文字を標準化または比較しやすくするためです。

## どうするんだ？
以下のコード例をご覧ください。

```Arduino
String str = "hello world";
str.toUpperCase();
Serial.println(str);  // HELLO WORLD
```
このとおり、"toUpperCase()"関数を使って文字列を大文字化できます。

## より深く知る
1. その歴史：大文字化は古くからある概念で、昔ながらのタイプライターや最初のコンピュータ用のASCIIコードでも見ることができます。
2. 他の方法：Arduinoでは"toUpperCase()"関数を使用しますが、他のプログラミング言語には異なる方法があります。例えば、Pythonでは".upper()"を使い、JavaScriptでは".toUpperCase()"と同じ関数名ですがJavaScriptではこの関数は文字列のコピーを返します。
3. 実装の詳細：アルファベットの大文字と小文字は、ASCIIコード上では32の違いがあります。したがって、大文字化の過程は、各文字に対して32を引くことで実現される場合が多いです。

## 関連する情報源
1. Arduino公式リファレンスのtoUpperCase関数 ([公式リファレンス](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/))
2. ASCIIコード ([ASCIIコード](https://www.asciitable.com/))
3. 文字列操作全般 ([関連記事](https://www.arduino.cc/en/Tutorial/StringManipulation))