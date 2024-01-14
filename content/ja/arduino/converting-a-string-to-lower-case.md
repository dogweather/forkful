---
title:    "Arduino: 文字列を小文字に変換する"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することは、プログラミングにおいて非常に便利な機能です。例えば、入力された文字列をすべて小文字に変換することで、処理をより簡単に行うことができます。

## 方法

```Arduino
// 入力された文字列を定義
String input = "Hello, World!";
// 文字列を小文字に変換する
String lowerCase = input.toLowerCase();
// 小文字に変換した文字列を出力
Serial.println(lowerCase);
```

上記のコードを実行すると、"hello, world!"という出力が得られます。このように、StringクラスのtoLowerCase()メソッドを使うことで、簡単に文字列を小文字に変換することができます。

## 深堀り

文字列を小文字に変換するには、大文字と小文字のアルファベットのasciiコードを利用します。大文字のアルファベットのasciiコードは65から90までの数字に対応しており、小文字のアルファベットのasciiコードは97から122までの数字に対応しています。つまり、大文字のasciiコードに32を足すことで対応する小文字のasciiコードを得ることができます。このような仕組みを利用して、Arduinoで文字列を小文字に変換することができます。

## 参考リンク

- [Arduino公式サイト](https://www.arduino.cc/)
- [Arduino Stringクラスリファレンス](https://www.arduino.cc/en/Reference/StringObject)
- [ASCIIコード表](https://www.ascii-code.com/)