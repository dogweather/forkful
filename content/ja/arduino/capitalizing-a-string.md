---
title:                "Arduino: 文字列の先頭を大文字に変換する"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

アルドゥイーノプログラミングでは、文字列を大文字に変換することは非常に一般的なタスクです。例えば、テキストメッセージや電子メールのタイトルを大文字で表示したり、ボタンのラベルを大文字で表示したりすることができます。文字列を大文字にすることで、テキストの読みやすさや視覚的な統一性を向上させることができます。

## 方法

文字列を大文字に変換する方法はいくつかありますが、ここでは最も一般的な方法を紹介します。まず、`toUpperCase()`という関数を使います。この関数は、文字列を大文字に変換した新しい文字列を返します。下の例を見てみましょう。

```
ArduinoString myString = "hello world";
ArduinoString upperString = myString.toUpperCase();
```
```
"HELLO WORLD"
```

このように、`toUpperCase()`関数を使うことで、元の文字列を変更せずに大文字に変換することができます。また、`toUpperCase()`関数は英数字だけでなく、日本語の文字列も大文字に変換することができます。

## ディープダイブ

文字列を大文字に変換する方法は、C言語と同じように`toupper()`関数を使う方法もありますが、`toUpperCase()`関数の方がArduinoプログラミングにおいてはより簡単で便利です。`toUpperCase()`関数は、内部で`toupper()`関数を使用しており、文字列内のすべての文字を大文字に変換します。また、文字列を大文字に変換する際には、文字エンコーディングにも注意する必要があります。日本語の場合、UTF-8の場合は問題なく大文字に変換されますが、Shift-JISなどのエンコーディングでは変換がうまく行われない場合もあります。

## おすすめリンク

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [C言語で文字列を大文字に変換する方法](https://www.tohoho-web.com/ex/case_up.html)
- [Arduinoでの文字エンコーディングの扱い方](https://tomosoft.jp/design/?p=13834)