---
title:                "文字列から引用符を削除する"
aliases:
- /ja/bash/removing-quotes-from-a-string/
date:                  2024-01-26T03:37:50.367333-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から引用符を取り除くことは、その文字列を囲んでいる引用符を削除することを意味します。プログラマーは、入力データを正規化するため、データを比較のために準備するため、または他のプログラムやシステムとのインターフェースを行う際に特定のデータフォーマットに従うために、これを行いたいと考えることがよくあります。

## 方法：
Bashには、文字列から引用符を取り除くいくつかの方法があります。ここに簡単な例をいくつか示します：

```Bash
#!/bin/bash

# 変数置換を使ってシングルクオートとダブルクオートの両方を取り除く
STRING="\"Hello, World!\""
echo ${STRING//\"}

# `tr` を使って引用符を削除する
STRING="'Hello, World!'"
echo $STRING | tr -d "\'"

# `sed` を使って引用符を削除する
STRING="\"Hello, World!\""
echo $STRING | sed 's/"//g'
```

出力例：

```
Hello, World!
Hello, World!
Hello, World!
```

## 詳細解説
昔、Unixコマンドの `tr` や `sed` はテキスト処理の主要なツールでした。引用符の削除のようなテキスト変換を扱うための柔軟性と強力さで、今日もなお使用されています。それらはどんなシェルスクリプターのツールボックスにも欠かせないものです。

Bash自体は進化し続けており、変数置換は小規模な文字列操作のための別の簡単さの層を追加しています。外部バイナリへのパイプアウトからあなたを救い、スクリプトを少し効率的にします。

`tr` は文字を削除するのに適していますが、より複雑なパターンを扱うことはできません。一方で `sed` は正規表現を使用するので、時にはオーバーキルになり、シンプルな操作には遅くなることがあります。

これらの方法の選択は、あなたの特定のケースに依存します。様々な引用符を取り除く必要があり、すでにBashスクリプトのコンテキスト内にいる場合、その簡単さのために変数置換を使用するのは非常に意味があります。しかし、テキストストリームや複数行のデータを変換する場合は、`tr` と `sed` があなたの親友です。

## 参照：
- GNU Bashマニュアル、特にパラメータ展開とシェルパラメータ展開のセクションについて：https://www.gnu.org/software/bash/manual/
- `tr` コマンドマニュアル：https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` ストリームエディタの概要：https://www.gnu.org/software/sed/manual/sed.html
