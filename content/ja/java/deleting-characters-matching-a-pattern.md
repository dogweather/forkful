---
title:                "Java: パターンに一致する文字を削除する"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ削除するのか？
基本的なJavaプログラミングの知識を持っている方なら、文字列や文字の比較に関する課題に直面したことがあるかもしれません。その中でも、特定のパターンにマッチする文字を削除することは、プログラマーにとってよく起こることです。例えば、検索エンジンのフォームに入力した文字列から不要な文字を削除するなど、実際のアプリケーションでもよく使われます。そこで、今回はこの「文字の削除」について、具体的な方法を紹介します。

## 削除のやり方
文字の削除は、ループや特定のメソッドを使うことで実現できます。まずは文字列を作成し、その中から特定のパターンにマッチする文字を探し、条件に合致する文字を削除します。以下の例は、正規表現を使用して特定のパターンにマッチする文字を削除するコードです。

```Java
String text = "apple, orange, banana, pineapple";
String filteredText = text.replaceAll("[aeiou]", "");
System.out.println(filteredText);


// Output: ppl, rng, bnn, pnnpl
```

上記のコードでは、`replaceAll()`メソッドを使用し、正規表現を使って文字列から「a, e, i, o, u」の文字を削除しています。ループを使わずに簡単に文字の削除ができますね。

## 詳しい解説
正規表現を使った文字の削除方法を詳しく見ていきましょう。正規表現とは、特定のパターンを表すための言語です。例えば、上記のコードでは`[aeiou]`という表現を使いましたが、これは「a, e, i, o, u」のいずれかの文字を表します。このように、正規表現を使うことで特定のパターンにマッチする文字を一括で置き換えることができます。

さらに、正規表現では「^」を使うことで文字の反転や「-」を使うことで範囲指定など、さまざまな表現が可能です。詳しい使い方は、正規表現に関するリンクを参考にしてください。

# その他の参考リンク
- [正規表現入門](https://qiita.com/kounoike/items/465513d5ab4e32e0caac)
- [Stringクラスのメソッド一覧](https://docs.oracle.com/javase/jp/11/docs/api/java.base/java/lang/String.html)
- [Java正規表現チュートリアル](https://www.tutorialspoint.com/java/java_regular_expressions.htm)

# 参考になる情報を見てみよう！