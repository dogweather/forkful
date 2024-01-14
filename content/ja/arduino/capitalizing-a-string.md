---
title:                "Arduino: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラムで文字列を大文字にすることのメリットについて、ご存知でしょうか？例えば、データやメッセージを処理する際に、文字列を統一的な形式にすることで処理しやすくなります。

## 使い方

文字列を大文字にする方法はいくつかありますが、今回は「Arduino」での方法をご紹介します。下のコードをご覧ください。

```Arduino
String str = "hello world";
str.toUpperCase();
Serial.println(str); // "HELLO WORLD"
```
ここでは、文字列を`String`オブジェクトとして宣言し、`toUpperCase()`メソッドを使って大文字に変換しています。そして、`Serial`を使って`str`をシリアルモニターに出力しています。

もし変換したい文字列が`str`以外にある場合は、代わりに`.toUpperCase()`メソッドの中に変換したい文字列を入力することもできます。

## 詳細説明

文字列を大文字に変換するには、コンピューターが文字のコードを変換する必要があります。それぞれの文字には、コンピューターが認識できるコード（ASCIIコードやUnicode）が割り当てられています。大文字と小文字は、このコードの値が異なるだけで同じ文字として扱われています。

`String`オブジェクトを使用することで、文字列を簡単に扱うことができます。また、大文字に変換するような基本的な操作も提供されています。

## 関連リンク

- [Arduino公式ウェブサイト](https://www.arduino.cc/)
- [C言語で文字列を大文字にする方法](https://programming-place.net/2020/04/13/how-to-uppercase-string-in-c/)