---
title:                "「文字列の大文字化」"
html_title:           "Arduino: 「文字列の大文字化」"
simple_title:         "「文字列の大文字化」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何をする & なぜ?
 文字列を大文字にするとは、プログラマーが文字列のアッパーケースを変更することを指します。これは、特定のデータフォーマットや表示の方法を必要とする場合に便利です。

## 方法:
```
Arduino String変数をアッパーケースにするためのコード例:
String name = "arduino";
Serial.println(name.toUpperCase());
```
出力: "ARDUINO"

## 詳しく見る:
- アルファベット以外の文字にも適用できるかどうかは、プログラム言語や環境によって異なります。
- 代替手段として、文字列のキャピタライズを行うための独自の関数を作成することもできます。
- 実装の詳細については、使用するプログラミング言語や環境のドキュメンテーションを確認することが重要です。

## 関連情報:
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- https://www.w3schools.com/cppref/cpp_ref_toupper.asp