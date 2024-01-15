---
title:                "「文字列の結合」"
html_title:           "Arduino: 「文字列の結合」"
simple_title:         "「文字列の結合」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# なぜ？
プログラミングで文字列を連結することの最大の利点は、複数の変数や値を一つの文字列として利用できることです。また、繰り返しコードを書く必要がなくなり、より効率的なコーディングが可能になります。

## やり方
文字列を連結するには、 + 演算子を使用します。例えば、"Hello"と"world"を連結するには、"Hello" + "world"と書きます。また、変数や定数と連結することもできます。

```Arduino
String str1 = "Hello";
String str2 = "world";
String result = str1 + " " + str2;
Serial.println(result); // Output: "Hello world"
```

## 深堀り
プログラムで文字列を連結する際に注意すべき点があります。まず、 + 演算子は文字列の結合であって、数値の計算ではありません。そのため、数値を文字列として連結する場合は、数値をString()関数で囲む必要があります。

また、文字列を連結する際にはメモリの使用量にも注意が必要です。大量の文字列を連結すると、メモリを消費することがあり、プログラムの動作に影響を与える可能性があります。そのため、必要以上に連結するのではなく、効率的にプログラムを設計することが重要です。

# 参考リンク
- [Arduino Reference - String Concatenation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [The "+" Operator](https://www.arduino.cc/en/Reference/StringConCat)
- [Common Pitfalls when Using String](https://www.arduino.cc/en/Tutorial/StringConstructors)