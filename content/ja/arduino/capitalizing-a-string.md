---
title:                "文字列の大文字化"
html_title:           "Arduino: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字に変換することの重要性や有用性を2文以内で説明します。

## 使い方
```Arduino
// 文字列を大文字にする例
String str = "hello world";
str.toUpperCase();
Serial.println(str); // HELLO WORLD
```

```Arduino
// シリアルモニターに大文字の入力を求める例
Serial.println("Enter a string:");
while (Serial.available() == 0); // ユーザーの入力があるまで待機
String input = Serial.readString(); // ユーザーの入力を読み取る
input.toUpperCase(); // 入力を大文字に変換
Serial.println(input); // 変換した文字列を出力
```

## 深堀り
文字列を大文字に変換するには、文字列オブジェクトのtoUpperCase()メソッドを使用します。このメソッドは、元の文字列を変更せずに大文字に変換した新しい文字列を返します。また、文字列の一部だけを大文字に変換するためには、文字列を一度リストや配列に分割し、必要な部分を大文字に変換して結合する方法があります。

## また見る
- [String クラスリファレンス](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCIIコード表](https://www.asciitable.com/)