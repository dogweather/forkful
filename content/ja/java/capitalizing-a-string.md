---
title:                "文字列を大文字にする"
html_title:           "Java: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 文字列の大文字化: Javaプログラミング

## 何と何故？

文字列の大文字化とは、文字列の中のすべての文字を大文字にすることです。プログラマーがこれを行う主な理由は、ユーザーが入力した文字列を正規化し、ケースに依存しない検索や比較を可能にするためです。

## やり方:

Javaでの文字列の大文字化は非常に簡単です。`toUpperCase()`メソッドを用いて、以下のように行います。

```Java
String str = "Hello, World!";
String upperStr = str.toUpperCase();
System.out.println(upperStr);  // Outputs: "HELLO, WORLD!"
```

このコードの実行結果は"HELLO, WORLD!"となります。

## 深掘り：

文字列の大文字化は、古くから存在する基本的なプログラミングのタスクで、多くのプログラミング言語でサポートされています。Javaでは、`toUpperCase()`メソッドがこのタスクを実行します。

このメソッドは内部でUnicode文字列を処理し、すべての文字を対応する大文字に変換します。このメソッドはlocaleに依存するバージョンと依存しないバージョンの2つが存在します。localeに依存するバージョンは特定の言語環境の大文字と小文字のルールを適用します。

Java言語以外でも、文字列の大文字化は通常、組み込み関数やメソッドによって簡単に行うことができます。たとえば、Pythonでは`upper()`メソッド、JavaScriptでは`toUpperCase()`メソッドを使用します。

## 関連情報：

* OracleのJavaドキュメンテーション: [Stringクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html)
* Stack Overflow: ["How to convert a string to all caps in java?"](https://stackoverflow.com/questions/5054995/how-to-convert-a-string-to-all-caps-in-java)
* Baeldung: ["Convert String to UpperCase in Java"](https://www.baeldung.com/java-convert-string-to-uppercase) 
* geeksforgeeks.org: ["String toUpperCase() Method in Java"](https://www.geeksforgeeks.org/java-lang-string-touppercase-java/)