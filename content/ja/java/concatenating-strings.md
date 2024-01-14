---
title:                "Java: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
1行または2行で、なぜ文字列を連結する必要があるのかを説明します。

Javaでプログラミングする際、文字列を連結する必要があることがあります。例えば、データベースから取得した複数のカラムの値を一つの文字列として表示したい場合や、ユーザーから入力した情報を一つの文章として保存する場合などです。文字列を連結することで、複数の情報を統一的な形で処理できるようになります。

## 方法
以下のコードブロック内に示されるように、Javaで文字列を連結する方法をご紹介します。

``` Java
// 文字列を "+" で連結する方法
String name = "太郎";
String greeting = "こんにちは、";
String message = greeting + name;
System.out.println(message); // 出力: こんにちは、太郎
```

``` Java
// Stringクラスのconcat()メソッドを使用する方法
String firstName = "太郎";
String lastName = "山田";
String fullName = firstName.concat(lastName);
System.out.println(fullName); // 出力: 太郎山田 
```

上記のように、文字列を連結する方法はいくつかありますが、最も一般的なのは「+」演算子を使用する方法です。また、Stringクラスには、concat()メソッドが用意されており、こちらを使用することでも文字列を連結することができます。どちらの方法を使用するかは、状況や好みによって異なりますので、都度判断する必要があります。

## 深堀り
文字列を連結する際には、文字の数が多いほど処理に時間がかかるということを知っておく必要があります。なぜなら、Javaでは文字列は不変(immutable)なオブジェクトとして扱われるため、文字列を連結する際には元々の文字列のデータを保持したまま新しい文字列を作成するためです。そのため、文字列を連結する回数が多い場合は、パフォーマンスの低下が生じる可能性があります。

そのような場合には、StringBuilderクラスを使用することで文字列の連結を効率的に行うことができます。StringBuilderクラスは可変(mutable)なオブジェクトであり、文字列の連結処理を行う際には元々の文字列を保持せずに文字列の追加を行うことができるため、パフォーマンスの向上が期待できます。

以上のように、文字列を連結する際には処理速度やメモリーの使用量を考慮する必要がありますので、適切な方法を選択するようにしましょう。

## はおりよこ

また、文字列を連結する方法以外にも、Javaで文字列を処理する際に役立つ情報をご紹介します。

- [Stringクラスのドキュメント (Oracle公式サイト)](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html)
- [StringBuilderクラスのドキュメント (Oracle公式サイト)](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/StringBuilder.html)
- [Javaでの文字列操作について (Qiita)](https://qiita.com/kmwork/items/a3d5c6a7f516556a58ca)