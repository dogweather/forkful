---
title:                "Arduino: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換する必要があるのか？これはプログラミングにおいてとても便利な機能です。たとえば、入力された文字列が大文字でも検索や比較を行いやすくするため、または出力の一貫性を保つために使用されます。コード上で文字列を小文字に変換することで、より正確な結果が得られるでしょう。

## 方法
文字列を小文字に変換するには、次のようなコードを使用します。

```Arduino
// 変換前の文字列を定義
String str = "HELLO WORLD";
// 小文字に変換
str.toLowerCase();
// 変換後の文字列を出力
Serial.println(str);
```

上記のコードを実行すると、シリアルモニターに"hello world"という出力が表示されます。このように、`toLowerCase()`メソッドを使用することで、文字列を簡単に小文字に変換することができます。

## 深堀り
Arduinoの`String`クラスには、さまざまなメソッドが用意されています。その中には、文字列を大文字や小文字に変換する`toUpperCase()`メソッドや`toLowerCase()`メソッドもあります。これらのメソッドは、ASCII文字の範囲内でのみ機能します。つまり、アルファベットや数字に限定されるということです。そのため、日本語などのマルチバイト文字や特殊文字は変換できません。

また、文字列を変換する際には、元の文字列を上書きしてしまうことに注意が必要です。上記の例では、`toLowerCase()`メソッドを使用した後、元の文字列`"HELLO WORLD"`は小文字になります。そのため、必要に応じて新しい変数に代入するなどして、元の文字列を保持するようにしましょう。

## 住みも見てください
# 同様の記事を読んでみる：
- [ArduinoのStringクラスのメソッドリファレンス（英語）](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [ASCIIコード（英語）](https://en.wikipedia.org/wiki/ASCII)