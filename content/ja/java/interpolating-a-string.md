---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ

文字列の内挿は、文字列に変数を埋め込む方法です。プログラマーは、動的に変わる値を文字列内で表示したい場合に行います。

## 実行方法

Javaでは「String.format」メソッドか「MessageFormat」クラスを使用して内挿を行います。

```Java
// String.format method
int apples = 5;
String text = String.format("I have %d apples.", apples);
System.out.println(text);

// MessageFormat class
import java.text.MessageFormat;

int bananas = 7;
String text2 = MessageFormat.format("I have {0} bananas.", bananas);
System.out.println(text2);
```

出力結果:

```
I have 5 apples.
I have 7 bananas.
```

## 深入り

- 歴史的な背景：初期のプログラミング言語では、文字列内挿はサポートされていなかったため、プログラマーは文字列結合を利用しました。しかし、Java 1.5からこの機能が導入されました。
- 代替方法：Java 8以降では、`java.util.Formatter` クラスや `printf` メソッドを利用することも可能です。
- 実装の詳細：`StringTemplate` や `apache-commons-text` ようなライブラリを利用すると、より高度な内挿を行うことも可能です。

## 参考情報

- OracleのJavaドキュメンテーション（文字列操作）: https://docs.oracle.com/javase/tutorial/java/data/strings.html
- Apache Commons Text（Javaの文字列操作ライブラリ）: https://commons.apache.org/proper/commons-text/
- Stack Overflow（文字列内挿についての質問と回答）: https://stackoverflow.com/questions/3655424/string-interpolation-in-java