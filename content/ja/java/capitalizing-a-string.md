---
title:                "Java: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の大文字化に取り組む理由を説明します。

## 方法

文字列を大文字化するには、まず文字列を取得し、それを`toUpperCase()`メソッドを使用して大文字に変換します。その後、新しい文字列を返すために、`toUpperCase()`メソッドを使用します。

```Java
// 文字列を取得
String originalString = "hello world";

// `toUpperCase()`メソッドを使って大文字に変換
String capitalizedString = originalString.toUpperCase();

// 変換後の文字列を出力
System.out.println(capitalizedString);
```

出力:
```
HELLO WORLD
```

## 深堀り

文字列を大文字化することは、プログラマーにとって非常に重要です。多くの場合、ユーザーからの入力を扱う場合、大文字と小文字を区別しないようにしたい場合があります。また、文字列の比較やソートを行う際にも大文字と小文字を区別しない方が便利です。

Javaでは、`toUpperCase()`メソッドの他にも、`toLowerCase()`メソッドを使用することで文字列を小文字化することもできます。また、`equalsIgnoreCase()`メソッドを使用することで、大文字と小文字を無視して文字列を比較することもできます。

## その他の記事

[Java Stringクラスのチュートリアル](https://www.javatpoint.com/java-string)、[文字列の大文字と小文字を変換する方法](https://www.geeksforgeeks.org/convert-string-lowercase-uppercase-java/)、[Java `toUpperCase()`メソッドのドキュメンテーション](https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/lang/String.html#toUpperCase()）

 _See Also:_

_関連記事:_

- [Javaの文字列とその操作](https://www.tutorialspoint.com/java/java_strings.htm)
- [Javaで大文字化と小文字化を扱う方法](https://www.baeldung.com/java-capitalize-strings)