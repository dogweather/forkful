---
title:                "Bash: 部分文字列を抽出する"
simple_title:         "部分文字列を抽出する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出することには、リストやテキストの特定の部分を取得する必要があるため、便利な場合があります。たとえば、特定の単語や文字列を検索する、またはテキストをカテゴリごとに分類する場合などがあります。

## 方法

サブストリングを抽出するには、Bashの組み込みコマンドである`cut`を使用します。以下のような形式で使用します。

```Bash
cut -(options) filename
```

`cut`コマンドは、指定したオプションに応じて、渡されたファイルからサブストリングを切り出します。たとえば、次のコマンドは、スペースで区切られたテキストの2番目の単語を取得します。

```Bash
cut -d " " -f 2 filename
```

また、`cut`コマンドは標準入力からもデータを受け取ることができます。例えば、`echo`コマンドを使用して文字列を出力し、それを`cut`コマンドにパイプしてサブストリングを抽出することもできます。

```Bash
echo "Hello World" | cut -d " " -f 2
```

上記の例では、"Hello World"という文字列から空白区切りで2番目の単語である"World"が抽出されます。

## ディープダイブ

`cut`コマンドでは、オプションを組み合わせることで、より複雑なサブストリングの抽出が可能です。例えば、`-c`オプションを使用すると、文字列の指定した位置や範囲の文字を抽出できます。

また、正規表現を使用することで、より柔軟なパターンにマッチするテキストを抽出することも可能です。

さらに、`cut`コマンドには、`-s`オプションを使用することで、指定した位置に文字列が存在しない場合には表示しないという機能もあります。

## 参考

「Bashでサブストリングを抽出する方法」: https://opensource.com/article/19/4/extracting-substrings-bash

「初心者のためのLinuxコマンド: cut」: http://www.lpi.org/blog/2014/04/23/learn-linux-101-cut

「Bashから文字列を取得する方法」: https://www.tecmint.com/bash-extract-filename-and-extension-in-linux/

## 関連記事

「Bashでの文字列操作の基本」: https://dev.classmethod.jp/articles/bash-string-process/

「Bashで正規表現を使用する方法」: https://pierrecarion.com/2018/01/extract-strings-linux/