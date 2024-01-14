---
title:    "Arduino: 文字列を小文字に変換する"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することに関心があるのであれば、これはあなたにとって便利な記事です。Arduinoでは、文字列を小文字に変換することができるため、プロジェクトにより独創性を与えることができます。

## 方法
Arduinoで文字列を小文字に変換するための簡単な方法は、 `toLowerCase()`関数を使用することです。以下に例を示します。

```Arduino
String myString = "HELLO WORLD";
myString.toLowerCase();
```

出力：hello world

`toLowerCase()`関数は、文字列に含まれるすべての大文字を小文字に変換します。

## ディープダイブ
`toLowerCase()`関数は、実際には文字列を変更しません。その代わりに、変更後の文字列を返します。したがって、変換したい文字列を変数に代入してから、`toLowerCase()`関数を使用する必要があります。

また、日本語の文字列を小文字に変換する際には注意が必要です。Arduinoでは、UTF-8エンコーディングを使用するため、カタカナやひらがななどの日本語の文字を小文字に変換することはできません。しかし、外部ライブラリを使用することで、日本語の文字列を正しく小文字に変換することができます。

## 参考リンク
- [ArduinoのStringオブジェクトリファレンス](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject)
- [ArduinoのtoLowerCase()関数のドキュメント](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/tolowercase/)
- [外部ライブラリで日本語の文字列を正しく小文字に変換する方法](https://translate-content.com/articles/tips/arduino-string-case-line/)

## 参考文献
[外部ライブラリで日本語の文字列を正しく小文字に変換する方法](https://translate-content.com/articles/tips/arduino-string-case-line/)