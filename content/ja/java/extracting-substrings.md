---
title:                "文字列の抽出"
html_title:           "Java: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何が、なぜ？

抽出された部分文字列とは何かを説明するために、私たちが普段のプログラマーで使っていることから始めましょう。部分文字列とは、大きな文字列から少しの部分を切り取ることであり、特定の用途に合わせた処理をするために使われます。例えば、電話番号やメールアドレスなど、特定のパターンを持つ文字列を抽出したい場合、部分文字列を使うことで簡単に処理することができます。

## 方法：

まずは、簡単なコード例を見てみましょう。以下のコードは、文字列から部分文字列を抽出する方法を示しています。

```java
String str = "これは私の部分文字列です。";
// "私の"という部分文字列を抽出する
String substring = str.substring(4, 7);
System.out.println(substring); // "私の"が出力される
```

上記のコードでは、`substring()`メソッドを使い、抽出したい部分文字列の開始位置と終了位置を指定しています。部分文字列は、開始位置を含め、終了位置を含まないことに注意しましょう。また、上記の例では`4`と`7`を指定していますが、実際には`0`から始まるインデックスを使い、`3`と`6`を指定することになります。

抽出する部分文字列の位置を指定するだけでなく、指定した位置に応じて部分文字列の長さを自動的に決めることもできます。

```java
String str = "これは私の部分文字列です。";
// "私の"という部分文字列を抽出する
String substring = str.substring(4);
System.out.println(substring); // "私の部分文字列です。"が出力される
```

上記の例では、部分文字列の終了位置を指定せず、開始位置のみを指定しています。そのため、指定した位置から最後までの部分文字列が自動的に抽出されます。

## 詳細

部分文字列を抽出する方法は、Java言語以外にも多くのプログラミング言語で利用することができます。例えば、Pythonでは`str[start:stop]`のように記述することで文字列の一部を抽出することができます。また、正規表現を使うことで、より柔軟に部分文字列を抽出することも可能です。

さらに、部分文字列を抽出する際には文字列のインデックスや長さの計算に関する注意が必要です。文字列の長さを超えた位置を指定した場合、エラーが発生することがあります。また、文字列の長さが変化する可能性がある場合にはプログラム全体のロジックを慎重に考える必要があります。

## 関連情報

- [String - Java Documentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#substring-int-int-)
- [String slicing in Python](https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-python-3)
- [正規表現入門](https://qiita.com/jo7ueb/items/dacb742fbf5a4a4a5488)