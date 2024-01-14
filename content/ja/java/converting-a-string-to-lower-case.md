---
title:                "Java: 文字列を小文字に変換する"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

なぜ文字列を小文字に変換する必要があるのかと思うかもしれません。それは、入力された文字列が大文字や混在文字であっても、データを比較や操作する際に統一性を保つために必要です。

## 方法

まず、文字列を入力する変数を作成します。

```Java 
String input = "Hello World";
```

次に、input変数を小文字に変換するメソッドを使用します。

```Java 
String output = input.toLowerCase();
System.out.println(output);
```

出力は、`hello world`となります。

## 深堀り

文字列を小文字に変換するメソッドは、`toLowerCase()`です。このメソッドは、文字列をすべて小文字に変換し、新しい文字列として戻り値を返します。元の文字列は変更されません。

ただし、このメソッドは英字のみに対応しています。日本語のようなマルチバイト文字では、期待通りの動作をしないことがあります。その場合は、`toLowerCase(Locale.JAPANESE)`のように、指定のロケールを使用してください。

## 参考文献

- [Java String toLowerCase() Method](https://www.w3schools.com/java/ref_string_tolowercase.asp)
- [Java マルチバイト文字とロケール](https://docs.oracle.com/javase/jp/6/api/java/util/Locale.html)

## 参考になるリンク

- [Markdownファイルを作成する方法](https://ja.wikipedia.org/wiki/Markdown)
- [Javaの文字列操作について学ぶ](https://www.geeksforgeeks.org/string-class-in-java/)