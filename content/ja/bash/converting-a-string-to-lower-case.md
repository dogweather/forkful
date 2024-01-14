---
title:                "Bash: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換するのには、様々な理由があります。例えば、入力された文字列を大文字と小文字の区別なく処理する必要がある場合や、文字列を比較する際にケースの違いを無視する必要がある場合などです。

## 方法

文字列を小文字に変換するためには、Bashで組み込みの `tr` コマンドを使用します。以下のようなコードを実行することで、文字列を小文字に変換することができます。

```Bash
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
```

このコードでは、`tr` コマンドの第一引数に大文字の文字集合を、第二引数に小文字の文字集合を指定しています。そして、パイプを使って`echo` コマンドで出力した文字列を `tr` コマンドに渡しています。実行結果は以下のようになります。

```Bash
hello world
```

## 深堀り

`tr` コマンドは、文字列を変換するために広く使われていますが、基本的な機能に加えて多様なオプションが存在します。例えば、`-d` オプションを使用することで、指定した文字を削除することができます。また、複数の文字集合を指定することで、複数の文字を同時に変換することも可能です。

さらに、`tr` コマンドはラテン文字だけでなく、マルチバイト文字やUnicode文字の変換にも利用することができます。詳細な使い方やオプションについては、マニュアルページを参照することをおすすめします。

## 併せて参照

- [Bashの`tr`コマンドのマニュアルページ](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Bashでの文字列操作について知る](https://dev.classmethod.jp/articles/bash-string-manipulation/)
- [Linuxコマンドの基本：文字列を置換する`tr`コマンド](https://gihyo.jp/admin/serial/01/linux_contest/0024)
- [Bashのリファレンスマニュアル](https://www.gnu.org/software/bash/manual/bash.html)