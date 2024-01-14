---
title:    "Java: 「文字列を小文字に変換する」"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換するのに関わるメリットや重要性について説明します。

## 方法
```Java 
//　文字列を定義
String str = "Hello WORLD";

// 文字列を小文字に変換
str = str.toLowerCase();

//　出力
System.out.println(str);
```
出力：hello world

## ディープダイブ
文字列を小文字に変換するには、まず文字列を定義し、その後に文字列を小文字に変換するメソッドを使用します。Javaでは、文字列は厳密なデータ型なため、変換によりバグを防ぐことができます。また、文字列を小文字に変換することで、文字列を比較する際にもより正確な結果を得ることができます。

## さらに参考になる情報
### 参考リンク
- [Javaで文字列を操作する方法](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Javaで文字列を比較する方法](https://www.geeksforgeeks.org/string-compareto-method-in-java/)
- [文字列の正規化についての解説](https://www.ibm.com/developerworks/jp/opensource/library/os-unicode/index.html)

## 良きJavaプログラマーへの一歩
この記事では、文字列を小文字に変換する方法を紹介しました。文字列を操作する際には厳密なデータ型であることを意識し、正確な結果を得るためにも小文字変換を積極的に使用しましょう。また、Javaには多くの文字列操作や比較に関するメソッドが存在するので、是非とも積極的に学習していきましょう。

（この記事を書いた人のプロフィールや連絡先などの情報を記載する。）