---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、大文字があれば、それを対応する小文字に変更する操作を指します。プログラマーは読みやすさのため、またはデータ一致の確認（大文字と小文字を区別せずに）などの理由からこれを行います。

## 使い方：

### bashを用いた文字列の小文字変換にはtrコマンドを使います。具体的な使い方は以下の通りです：

```Bash
$ echo "HELLO, WORLD!" | tr '[:upper:]' '[:lower:]'
hello, world!
```

上記コマンドでは、'HELLO, WORLD!'という文字列が全て小文字の'hello, world!'に変換されています。

## 深く見る：

### まずは歴史から：Bashは1979年のVersion 7 Unixに初めて導入されました。その後のバージョンであるBash 4.0以降では、内部コマンドを使用して文字列を小文字に変換することができます。

例：

```Bash
$ string="HELLO, WORLD!"
$ echo "${string,,}"
hello, world!
```

今回のテーマであるtrコマンドの方が歴史的には古く、さまざまなUNIX系システムで使われています。

### 変換方法の代替案としてはPythonやPerlを使う方法もあります。これらの言語の内部関数を使えば、より複雑な文字列操作も可能です。
   　　
実装について：Bashの場合、小文字変換は内部的にはCのtolower関数を通じて行われます。trコマンドの場合、ASCII範囲の値に基づく文字の変換が行われます。　　　　　　　　　　　　　　　　　　

## 参考情報：

以下は本テーマに関連する参考リンク集です：

1. Bashの公式ドキュメンテーション：https://www.gnu.org/software/bash/manual/bash.html
2. trコマンドの詳細：https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
3. Pythonでの文字列操作：https://docs.python.org/ja/3/library/string.html
4. Perlでの文字列操作：https://perldoc.perl.org/functions/lc.html 

このリンク集を参考に、さらに理解を深めていってください。