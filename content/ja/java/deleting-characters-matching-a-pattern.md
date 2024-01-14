---
title:    "Java: パターンと一致する文字を削除する"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ

あるパターンにマッチする文字を削除することに必要性を感じるのは、データから特定の文字列を取り除く必要があるときです。例えば、テキストから不要な空白を削除したり、特定の文字列を省略したりする場合に使用できます。

# 方法

文字列を扱うJavaで、パターンにマッチする文字を削除する方法は主に2つあります。一つはStringクラスのreplaceAll()メソッドを使用する方法、もう一つはStringBuilderクラスのdelete()メソッドを使用する方法です。以下のコードブロックを参考にしてください。

```Java
// replaceAll()メソッドを使用する場合
String str = "Hello World";
String newStr = str.replaceAll("o", ""); // "Hello Wrld"

// delete()メソッドを使用する場合
StringBuilder sb = new StringBuilder("Hello World");
sb.delete(4, 6); // StringBuilderの中身は "Helloorld" になる
```

上記の例では、"o"というパターンにマッチする文字を削除する方法を紹介しましたが、同様に正規表現を使用することで、より複雑なパターンにも対応できます。

# ディープダイブ

文字を削除する際には、削除される文字のインデックスや位置を理解することが重要です。StringクラスのreplaceAll()メソッドでは、全てのパターンにマッチする文字が一括で削除されるため、文字の位置がずれてしまう可能性があります。一方、StringBuilderクラスのdelete()メソッドでは、指定した範囲の文字が削除されるため、文字の位置は変わりません。また、正規表現を使用する際にはパターンの記述にも注意が必要です。詳しくは公式ドキュメントを参考にしてください。

# 併せて参照

- [JavaのStringクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [JavaのStringBuilderクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html#delete-int-int-)
- [正規表現の基礎知識](https://www.javadrive.jp/start/regex/index1.html)