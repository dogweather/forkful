---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
ランダムな番号を生成するとは、決まった規則や順番なく数値を出力することです。この機能は、予測不可能な結果や要素をプログラムに組み込むためによく使われます。

## 実装方法：
Fish Shellにおけるランダム数値の生成には`random`コマンドが用いられます。

例：

```fish
# 1 and 10 の間のランダムな数
random 1 10
```

出力例:

```fish
7
```

もう一つの例：

```fish
# ランダムな暗証番号を生成
random 0 9 (random 0 9) (random 0 9) (random 0 9)
```

出力例:

```fish
8 2 7 0
```

## ディープダイブ
Fish Shellの`random`コマンドは、2005年のリリース以来、シェルスクリプトにおけるランダムな数値生成のための主要な手段となりました。同様の機能は他のシェルにも存在しますが、Fishはその使いやすさで人気があります。

代替手段には、`$RANDOM`環境変数を利用する方法（Bashなど）があります。しかし、Fish Shellではこの方法はサポートされていません。

`random`コマンドの実装については、特定の範囲の数値を均等に生成するために、擬似乱数生成器（PRNG）という技術が用いられています。

## 参照
以下は、関連するリソースへのリンク集です：

1. Fish Shell公式ドキュメンテーション - [Random function](https://fishshell.com/docs/3.1/commands.html#random)
1. Stack Overflow - [Generating Random Numbers in Fish Shell](https://stackoverflow.com/questions/31159157/generating-random-numbers-in-fish-shell)

以上で、Fish Shellでのランダムな数値生成についての解説を終了します！