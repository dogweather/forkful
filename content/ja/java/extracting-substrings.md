---
title:                "部分文字列の抽出"
html_title:           "Java: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

文字列処理において、部分文字列の抽出は非常に重要です。プログラムを効率的かつ正確に動作させるためには、文字列データから必要な部分だけを取り出すことが必要です。そのため、Javaでは便利なメソッドが用意されており、部分文字列の抽出を簡単に行うことができます。

## How To

文字列から部分文字列を抽出するには、`substring()`メソッドを使用します。このメソッドは2つの引数を取り、第一引数は抽出したい部分文字列の開始位置のインデックス、第二引数は終了位置のインデックス（このインデックスは抽出される部分文字列には含まれません）を指定します。

例えば、文字列 "こんにちは、私はJavaです。" から "こんにちは、私は" の部分文字列を抽出するコードは以下のようになります。

```Java
String str = "こんにちは、私はJavaです。"; 
String extracted = str.substring(0, 8);
System.out.println(extracted);
```

抽出された部分文字列は変数 `extracted` に保存され、コンソールには "こんにちは、私は" という文字列が表示されます。

また、`substring()`メソッドは第一引数を省略することもできます。その場合、指定したインデックスから文字列の末尾までを抽出します。例えば、上記のコードを以下のように書き換えることもできます。

```Java
String str = "こんにちは、私はJavaです。"; 
String extracted = str.substring(0);
System.out.println(extracted);
```

`substring()`メソッドを使うことで、文字列の中から特定の部分を簡単に抽出することができます。

## Deep Dive

Javaでは、`substring()`メソッドの他にも、文字列の特定の部分を取得するためのメソッドがあります。例えば、`indexOf()`メソッドを使用すると、指定した文字列または文字のインデックスを取得することができます。これを組み合わせることで、より柔軟な部分文字列の抽出が可能になります。

また、Java 11から導入された`String#lines()`メソッドを使用すれば、文字列を行ごとに分割して配列に格納することができます。これを使うことで、複数の行にまたがる部分文字列を取得することができます。

さらに詳しい情報や、文字列処理における他の便利なメソッドについては、公式ドキュメントを参照することをおすすめします。

## See Also

- [Java - 文字列操作](https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/lang/String.html)
- [凝縮された Java 文字列操作チートシート](https://agilewarrior.wordpress.com/2012/06/27/a-shortened-java-string-manipulation-cheat-sheet/)