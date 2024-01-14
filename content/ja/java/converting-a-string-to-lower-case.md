---
title:                "Java: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングにおいて文字列を小文字に変換することのメリットは、文字列の一致を確認する際に大文字と小文字を区別しなくても良い点にあります。また、ソートや検索の処理を行う際にも、文字列を小文字に変換することでより簡単に処理することができます。

## 方法

文字列を小文字に変換する方法は、Javaプログラムで```toLowerCase()```メソッドを使用することです。以下の例を参考にしてください。

```Java
String str = "Hello World";
String lowerCaseStr = str.toLowerCase();

System.out.println(lowerCaseStr);
```

このコードを実行すると、出力は以下のようになります。

```Java
hello world
```

## 深堀り

文字列を小文字に変換するために使用される```toLowerCase()```メソッドは、Stringクラスに定義されています。そのため、Stringクラスを継承したクラスであれば、どのようなクラスでもこのメソッドを使用することができます。

また、英語以外の言語でも同じように動作します。つまり、日本語の文字列を小文字に変換する際にも同じメソッドを使用することができます。

## See Also

- [Java Stringクラス](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java toLowerCase()メソッドのドキュメンテーション](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Javaの基本教程(英語)](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/index.html)