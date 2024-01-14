---
title:                "Arduino: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを見つけることに関心を持つ理由は様々ですが、例えばユーザーに入力された文字列を制限するために使用する場合や、プログラミングチャレンジで必要となる場合があります。

## 方法

Arduinoを使用して文字列の長さを見つける方法はいくつかありますが、ここでは一つの例を紹介します。

```Arduino
// 文字列を定義
String myString = "こんにちは、世界！";
// 文字列の長さを出力
Serial.println(myString.length());
```

この場合、シリアルモニターには「13」という数字が表示されるでしょう。なぜならば日本語の「こんにちは、世界！」には全角の「！」が使用されており、英数字と同様の計算で考慮されるからです。

## ディープダイブ

文字列の長さを見つけるには内部でどのような処理が行われているのでしょうか？Arduinoでは、文字列を処理する際にバイト数を使用しています。バイト数とは、16進数の表示形式である「0x」が付いた数字のことです。例えば、日本語の文字「こんにちは」は「こんにちは」という6文字を表しますが、内部で使用されるバイト数は8バイトであり、省略された2バイトは日本語の文字を表すために必要なものです。

## 参考リンク

- [Arduinoで文字列の長さを調べる方法](https://yokoyama.ninja/how-to-find-string-length-in-arduino)
- [文字列のバイト数を求める方法](https://qiita.com/ktm/items/3a371726367c1ab965e7)