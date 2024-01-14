---
title:                "Arduino: テキストの検索と置換"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

プログラムを書いていると、文字列の中に間違った単語や誤字脱字があった場合、手作業で修正するのはとても面倒です。また、同じコードを何度もコピー＆ペーストして使用する際にも、複数の場所で同じ変数名や定数を使用したいと思うかもしれません。そのような場合、検索＆置換を使用すると、簡単かつ効率的に文字列を修正することができます。

## 方法

Arduinoでは、テキストエディタを使用して検索＆置換を行うことができます。以下のようなコードをテキストエディタに入力し、「Find and Replace」の機能を使用して文字列の一部を置換することができます。

``` Arduino
int ledPin = 9;
int switchPin = 2;
int buttonState = 0;
int pressedState = 1;

buttonState = digitalRead(switchPin);
if(buttonState == pressedState) {
  digitalWrite(ledPin, HIGH);
} else {
  digitalWrite(ledPin, LOW);
}
```

上記のコードを見ると、「int」や「digitalRead」、「if」といった単語が複数回使用されていることがわかります。これらの単語を一括で置換することができれば、複数の箇所で同じ変数や関数を使用しても、手間が少なくなります。

## ディープダイブ

検索＆置換はただ単語を置き換えるだけではありません。正規表現を使用することで、部分的に一致した文字列をすべて置き換えることも可能です。例えば、```.```という表現は任意の1文字に一致するので、``` myVar.```という文字列を``` myVar = 0;```という文字列に一括置換することができます。

さらに、コラムや行数などを指定することで、特定の範囲内の文字列のみを置換することもできます。これらの応用技術を使用することで、より詳細な検索＆置換を行うことができます。

## 参考になるリンク

- [Arduino公式サイト](https://www.arduino.cc/)
- [正規表現チートシート](https://www.debuggex.com/cheatsheet/regex/javascript)
- [TextWranglerの検索＆置換機能の使い方](https://www.codepile.net/pile/LdzQ0Q02)