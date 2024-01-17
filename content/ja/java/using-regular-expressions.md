---
title:                "正規表現を使用する"
html_title:           "Java: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

正規表現を使うことは、文字列のパターンを検索や置換することができるプログラミングの手法です。プログラマーは、効率的に大量のテキストデータを処理するために正規表現を使用します。

## 方法:

```Java
// 文字列の中から特定の単語を検索する例
String str = "これはサンプルテキストです。";
if(str.matches(".*サンプル.*")) {
    System.out.println("サンプルが見つかりました。");
}
```

```Java
// 正規表現を使って文字列を置換する例
String str = "Hello, World!";
str = str.replaceAll("World", "Universe");
System.out.println(str);
// 出力: Hello, Universe!
```

## 深堀り:

1. 正規表現は、1960年代に開発されたプログラミング言語の一つであるPerlに由来します。
2. 正規表現に代わる手法として、文字列操作やパターンマッチングの機能を提供するStringクラスのメソッドを使用する方法もあります。
3. 正規表現は、Javaで "java.util.regex" パッケージをインポートして使用することができます。

## 関連リンク:

- [Javaで正規表現を使用する方法](https://docs.oracle.com/javase/jp/7/docs/api/java/util/regex/Pattern.html)
- [正規表現のパターンをチェックするオンラインツール](https://regex101.com/)