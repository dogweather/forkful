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

# なぜ
文字列を小文字に変換することのメリットを最大2文で説明する。

Arduinoの現在のバージョンでは、文字列を小文字に変換する機能があります。これにはいくつかの理由があります。例えば、文字列を比較する場合に大文字と小文字を区別したくない場合や、ある特定の文字列操作を行うために必要な場合などがあります。

しかし、わざわざ自分で小文字に変換するのは手間がかかるため、Arduinoには文字列を小文字に変換する機能が組み込まれています。

## 方法
以下のようにArduinoのコードを書くことで、文字列を小文字に変換することができます。
```Arduino
String str = "HELLO WORLD";
str.toLowerCase(); // 文字列を小文字に変換する
```

上記の例では、大文字の"HELLO WORLD"が小文字の"hello world"に変換されます。ここで注意すべき点は、小文字に変換された文字列は新しい変数に代入されるのではなく、元の変数の値が変更されるということです。

## ディープダイブ
文字列を小文字に変換する際に、Arduinoがどのように処理を行っているかについて説明します。

Arduinoでは、文字列を小文字に変換するためにASCIIコード表を使用しています。ASCIIコード表とは、文字を数字で表現する方法です。大文字の"A"はASCIIコードで65、小文字の"a"は97に対応しています。そのため、Arduinoは文字列を小文字に変換する際に、文字のASCIIコードを差し引いて変換を行います。

また、Arduinoでは文字列を小文字に変換するための組み込み関数として「toLowerCase()」がありますが、同様に大文字に変換するための「toUpperCase()」関数も存在します。

## 参考リンク
- [Arduino Reference - String toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [ASCIIテーブル](https://ja.wikipedia.org/wiki/ASCII#ASCII.E3.83.86.E3.83.BC.E3.83.96.E3.83.AB)