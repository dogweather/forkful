---
title:                "文字列の長さを求める"
html_title:           "Arduino: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

アルドゥイーノを使う際、文字列の長さを知ることは重要です。文字列の長さを知ることで、必要なメモリの量を正しく計算することができます。

## 方法

Arduinoの`strlen()`関数を使うことで、使用する文字列の長さを求めることができます。以下の例を参考にしてください。

```Arduino
// 文字列の宣言
char str[] = "こんにちは";

// 文字列の長さを求める
int length = strlen(str);

// シリアルモニターに結果を出力
Serial.println(length);
```

上記のコードを実行すると、シリアルモニター上に「5」という数字が表示されるはずです。このように、`strlen()`関数を使うことで簡単に文字列の長さを求めることができます。

## さらに深く

`strlen()`関数は、空白を含めた文字の数を数えてくれます。しかし、日本語のように1文字が複数バイトで表現される言語の場合、`strlen()`関数では正確な文字数を数えることができません。そのため、`str.length()`を使うことでより正確な文字数を表示することができます。

## 参考リンク

- [Arduino Reference - strlen()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [知らないと損する文字列の長さを調べる方法](https://qiita.com/weedslayer/items/80e770a2ce5cb2961e53)
- [str.lengthとstrlenの違いと使い分け方](https://qiita.com/bus0614/items/e6a5375a1d54d197dfc2)

## 参考文献

- Arduino Reference - strlen(): https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/