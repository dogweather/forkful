---
title:    "Java: テキストの検索と置換"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ

テキストを検索して置換することの重要性は、コーディングプロセスを迅速化し、効率的に行うことができるからです。また、大量のコードを修正したい場合や、特定のキーワードを一括で変更したい場合にも役立ちます。

## 使い方

検索と置換を行う際には、JavaのStringクラスに用意されている`replaceAll()`メソッドを使用します。以下は、文字列の中で特定のキーワードを置換するためのコード例です。

```Java
String myString = "Javaはとても面白いです！";
String newString = myString.replaceAll("面白い", "楽しい");
System.out.println(newString);
```

出力結果は以下のとおりになります。

```
Javaはとても楽しいです！
```

上記の例では、`replaceAll()`メソッドを使用して`myString`の中から「面白い」を「楽しい」に置換し、新しい文字列を`newString`に代入しています。このように、置換するキーワードを事前に指定することで、コード内の特定の文字列を一括で置換することができます。

## 深堀り

文字列の検索と置換は、正規表現を使用することもできます。正規表現を使用することで、より柔軟な検索と置換が可能になります。また、正規表現を使用することで検索モードを「大文字小文字区別しない」や「繰り返し検索」に設定することもできます。

例えば、以下のコードでは、全ての数字を「#」に置換する正規表現を使用しています。

```Java
String myString = "Java 1.8 Tutorial";
String newString = myString.replaceAll("\\d", "#");
System.out.println(newString);
```

出力結果は以下のようになります。

```
Java #.# Tutorial
```

上記の例では、`replaceAll()`メソッドの第一引数には `\d` を使用して、数字を表す正規表現を指定し、第二引数には置換する文字を指定しています。このように、正規表現を使用することでより複雑な検索と置換が可能になります。

## さらに参考になる記事

- [正規表現の基本](https://www.javadrive.jp/start/regex/index1.html)
- [JavaのStringクラスのreplaceAll()メソッドの使い方](https://www.javadrive.jp/start/string/index14.html)
- [Javaで文字列の検索と置換をする方法](https://www.sejuku.net/blog/25352)