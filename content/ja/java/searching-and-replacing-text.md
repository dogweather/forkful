---
title:    "Java: テキストの検索と置換"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由について説明します。多くのプログラマーはデータやテキストを処理しなければならない時、検索と置換は非常に便利なツールです。例えば、大量のテキストデータを処理する必要がある場合や、特定の文字列を一括で変更する必要がある場合などに使われます。

## 方法

検索と置換は、Javaで簡単に実装することができます。まずは検索文字列を指定し、それに対して置換する文字列を指定します。そして、処理するテキストデータを指定して処理を実行します。以下に、コード例と実行結果を示します。

```Java
// 検索文字列と置換文字列を指定
String searchStr = "Hello";
String replaceStr = "こんにちは";

// 処理するテキストデータを指定
String text = "Hello World!";

// 文字列の置換を実行
String newText = text.replace(searchStr, replaceStr);

// 結果を表示
System.out.println(newText);
```

上記のコードの実行結果は、「こんにちはWorld!」となります。

## 深堀り

検索と置換は、多くの場面で使われるだけでなく、JavaのStringクラスにも多くの便利なメソッドが用意されています。例えば、大文字と小文字を区別せずに検索する「replaceAll()」メソッドや、正規表現を使ってパターンにマッチする部分を一括で置換する「replaceFirst()」メソッドなどがあります。これらのメソッドを利用することで、より複雑な処理も簡単に実装することができます。

## 参考文献

- [Oracle Java Stringクラスドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java 入門 - 検索と置換](https://java.keicode.com/lang/string-search-replace.php)
- [Javaで文字列を置換する方法](https://weblabo.oscasierra.net/java-string-replace/)