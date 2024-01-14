---
title:                "Bash: 部分文字列の抽出"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングの抽出を行う理由はさまざまです。その一つは、特定の文字列や単語を抽出してより効率的な処理を行うことができるからです。また、テキストデータの分析や検索を行う際にも便利です。

## 方法

サブストリングを抽出するには、以下の方法を使うことができます。

```
Bash
# 変数を定義
str="こんにちは、私の名前は太郎です。"

# 抽出したい文字列を指定してサブストリングを抽出
echo ${str:9:3}

# 出力結果: 太郎
```

## ディープダイブ

サブストリングを抽出する方法には様々なバリエーションがあります。例えば、文字列の開始位置や終了位置を指定したり、特定のパターンに一致する部分文字列を抽出することもできます。また、正規表現を使用してサブストリングを抽出することも可能です。

しかし、サブストリングを抽出する際には文字列のインデックス番号に注意する必要があります。Bashでは、文字列の1文字目が0番目となります。また、マイナスの数値を指定すると文字列の末尾からカウントされます。

## 関連リンク

* [Bash Guide for Beginners](https://guide.bash.academy/)
* [Bash Substring Manipulation](https://www.baeldung.com/linux/bash-substring)
* [Advanced Bash-Scripting Guide - Substring Expanding](https://www.tldp.org/LDP/abs/html/parameter-substitution.html#SUBSTREXPAND)