---
title:                "「文字列を小文字に変換する」"
html_title:           "Java: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何をするのか & なぜするのか？
ストリングをローワーケースに変換するとは、その名の通り、文字列の大文字を小文字に変換することです。プログラマーがこの作業を行う理由は、文字列の大小を問わず比較するために、文字列の一貫性を保証することができるからです。

## 方法：
```Java
String string = "Hello World";
string = string.toLowerCase();
System.out.println(string);
// Output: hello world
```

## 詳細を調べる
文字列をローワーケースに変換するメソッドは、JavaのStringクラスに既に存在しています。そのため、カスタムメソッドを作成する必要はありません。このメソッドは、現代のプログラミング言語では一般的な操作です。以前の古いプログラミング言語では、この機能がない場合もありましたが、今ではほとんどの言語で使えるようになっています。

文字列をローワーケースに変換する代替手段としては、正規表現やループ処理を使用する方法もありますが、JavaのStringクラスのメソッドを使う方が簡単で効率的です。

このメソッドの実装では、文字列内の各文字をASCIIコードを使って小文字に変換しています。そのため、英語や日本語のアルファベット以外の文字は変換されません。また、元の文字列は変更されず、変換後の文字列が新たに返されます。

## 関連情報を見る
- JavaのStringクラスのtoLowerCase()メソッドのドキュメント: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--
- 文字列操作についてのより詳しい情報: https://www.geeksforgeeks.org/string-class-in-java/
- Javaの正規表現について: https://www.javatpoint.com/java-regex