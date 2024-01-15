---
title:                "文字列を小文字に変換する"
html_title:           "Java: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することに興味を持つ理由はいくつかあります。例えば、データベースやファイル操作の際に大文字と小文字が区別されないため、文字列を比較する際に混乱を避けるためです。また、ユーザーからの入力を統一的に処理するためにも重要です。

## 変換方法

```Java
String str = "Hello World";
System.out.println(str.toLowerCase());
```
このコードの出力結果は "hello world" になります。また、JavaのStringクラスにはtoLowerCase()というメソッドがあり、これを使うことで文字列を小文字に変換することができます。

## 深堀り

文字列を小文字に変換するメソッドであるtoLowerCase()は、実際にはStringクラスのインスタンス変数であるvalueにアクセスして、文字列の中の各文字を小文字に変換しています。また、多言語対応のために、ロケールという情報を考慮して変換を行うことができます。

## 他に見る

- [JavaのStringクラスの公式ドキュメント](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Javaで文字列を操作する方法](https://www.javatpoint.com/java-string)
- [文字列の大小を無視して比較する方法](https://stackoverflow.com/questions/5901623/how-to-ignore-case-for-string-in-java)