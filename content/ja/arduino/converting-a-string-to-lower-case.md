---
title:                "「文字列を小文字に変換する」"
html_title:           "Arduino: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
文字列の大文字を小文字に変換することは、プログラマーにとって重要なことです。なぜなら、コンピューターは大文字と小文字を区別し、間違った結果を与えることがあるからです。そのため、文字列を正確かつ効率的に処理するために、大文字を小文字に変換する必要があります。

## How to:
Arduinoでは、stringオブジェクトである変数`myString`を使用して、次のように文字列を小文字に変換することができます。
```
Arduino
myString.toLowerCase();
```
このコードを実行すると、`myString`の値が全て小文字に変換されます。

例えば、文字列`"HELLO"`を小文字に変換すると、`"hello"`という文字列が結果として得られます。

## Deep Dive:
大文字を小文字に変換する方法は、コンピューターの歴史と共に発展してきました。昔は、大文字を小文字に変換するためには手動でアルファベット表を記憶し、大文字を小文字に変換する必要がありました。しかし、今ではプログラミング言語に組み込まれた関数を使用することで簡単に変換することができます。

また、大文字を小文字に変換する方法には複数のアルゴリズムがあります。最も一般的なものはASCIIコードを使用する方法で、コンピューターは英文字のASCIIコードを変換することで大文字を小文字に変換します。

## See Also:
- [String Library Reference (Arduino)](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [ASCII Code (Wikipedia)](https://en.wikipedia.org/wiki/ASCII)