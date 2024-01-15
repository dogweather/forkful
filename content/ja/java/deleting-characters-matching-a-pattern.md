---
title:                "パターンにマッチする文字を削除する"
html_title:           "Java: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字列の中から特定のパターンにマッチする文字を削除する理由について説明します。例えば、不要な文字を取り除いて不要な情報を排除することができるため、データのクレンジングに役立ちます。

## 方法

以下のコード例を参考に、Javaで文字を削除する方法を紹介します。

```Java
// 文字列を保存する変数を作成
String str = "Hello World!";

// 文字列から特定の文字を削除
String result = str.replaceAll("l", "");

// 結果を出力
System.out.println(result);
```

上記のコードを実行すると、以下のように出力されます。

```
Heo Word!
```

文字列の中から"l"という文字を全て削除し、不要な文字を取り除くことができました。

## ディープダイブ

文字列を削除する際に使用する主なメソッドには、`replaceAll()`や`replace()`などがあります。`replaceAll()`はパターンにマッチする文字列を全て置換し、`replace()`は最初にマッチした文字列のみを置換します。また、正規表現を使用することで、より複雑なパターンにマッチし、文字列を置換することも可能です。

## 関連リンク

- [Javaで文字列を置換する方法](https://programmingwithmosh.com/java/string-replace-methods/)
- [Javaで正規表現を使用する方法](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)