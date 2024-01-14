---
title:                "Java: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ？

文字列の長さを求めることの魅力は、その文字列をより効率的に操作するためです。例えば、文字列の一部を抜き出す場合や、特定の文字列を検索する場合に、文字列の長さが必要になります。

## やり方

文字列の長さを求めるには、Javaの組み込みメソッドである`length()`を使用します。以下のコードブロックを参考にしてください。


```Java
String myString = "こんにちは、世界！";
int length = myString.length();

System.out.println("文字列の長さは" + length + "です。");
```

このコードを実行すると、次の出力が得られます。

```Java
文字列の長さは9です。
```

## 深堀り

Javaの`length()`メソッドは、実際には`int`型のプリミティブデータ型を返します。これは、文字列のバイト数をカウントしているためです。そのため、日本語や他のマルチバイト文字を含む文字列の場合、意図した文字数とは異なる結果が得られる可能性があります。

また、`length()`メソッドはオブジェクトではなく、クラスの静的メソッドです。つまり、このメソッドを呼び出すときは、必ずクラス名を前に付ける必要があります。

## 参考リンク

- [JavaのStringクラスの公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html)
- [Javaで文字列を操作する方法](https://www.javadrive.jp/start/string/index1.html)
- [Javaの組み込みメソッド一覧](https://www.javatpoint.com/java-string-methods)
- [Javaで文字列を操作するためのライブラリー「Apache Commons Lang」](https://commons.apache.org/proper/commons-lang/)