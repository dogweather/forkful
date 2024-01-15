---
title:                "文字列の連結"
html_title:           "Java: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結させる理由はたくさんあります。例えば、複数の文字列を一つの文に組み合わせる必要がある場合や、出力結果を整形する際に文字列を連結する必要がある場合などが挙げられます。Javaでは、文字列の連結が簡単に行えるので、効率的なプログラミングをする際に重要なスキルとなります。

## 使い方

文字列を連結する方法はいくつかありますが、今回は `+` 演算子を使用する方法を紹介します。まずは、連結させたい文字列を `+` 演算子で結合します。例えば、次のようなコードを見てみましょう。

```Java
String name = "太郎";
String message = "こんにちは、" + name + "さん！";
System.out.println(message);
```

この場合、`+`演算子を使って`"太郎"`という文字列を`"こんにちは、"`と`"さん！"`の間に結合しています。実行すると、`こんにちは、太郎さん！`という結果が出力されます。

また、`"+"` 演算子を使用する際は、型の自動変換が行われるため、文字列以外のデータ型も自動的に文字列に変換されます。例えば、`"年齢"`という文字列と`32`という整数を結合する場合、`32`は自動的に文字列に変換されて`"32"`となり、`"年齢"`と一緒に表示されるようになります。これにより、簡単に複数のデータを連結できるようになります。

## ディープダイブ

文字列を連結させる方法は他にもいくつかあります。`String`クラスの`concat()`メソッドや、`StringBuffer`クラスの`append()`メソッドを使う方法などもあります。また、文字列を連結する際には、文字列の変更が頻繁にある場合は`StringBuffer`クラスを使用し、変更がない場合は`String`クラスを使用するとパフォーマンスが向上します。また、文字列の連結を行う際には、不要なオブジェクトを作成しないよう気をつけることが重要です。これらの詳細な情報を知りたい方は、[Oracleの公式ドキュメント](https://docs.oracle.com/javase/tutorial/java/data/strings.html)を参考にしてください。

## 参考リンク

- [Javaにおける文字列の連結方法について（日本語）](https://java-reference.com/java_string_concat.htm)
- [Stringクラス（Java API Documentation）](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html)
- [StringBufferクラス（Java API Documentation）](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/StringBuffer.html)
- [Oracleの公式ドキュメント（英語）](https://docs.oracle.com/javase/tutorial/java/data/strings.html)