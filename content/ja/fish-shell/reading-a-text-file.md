---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:54:05.279920-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

category:             "Fish Shell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
テキストファイルを読み込むとは、ファイルの内容をメモリに取り込むことです。プログラマーは情報を処理、解析、または表示するためにこれを行います。

## How to (やり方):
```fish
# テキストファイルの内容を表示する
cat file.txt

# ファイルから読み込んだ行を1行ずつ処理する
while read -la line
    echo $line
end < file.txt
```

サンプル出力:
```fish
これはファイルの一行目です
これは二行目です
```

## Deep Dive (深掘り):
テキストファイルを読むことはプログラミングの古典的なタスクです。UNIX系システムでは、'70年代から`cat`, `more`, `less`などのコマンドで行われています。Fish Shell はこれらのコマンドを利用しますが、操作をより直感的にしています。バイナリファイルを読む代わりに、`xxd` や `hexdump` のようなツールがあります。Fish の `read` コマンドはシェルスクリプト内でファイルの内容を行ごとに読み取り、変数に代入しやすくします。

## See Also (関連情報):
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- GNU Core Utilities Documentation for `cat`: https://www.gnu.org/software/coreutils/cat
- A tutorial on text processing in Unix shells: https://www.grymoire.com/Unix/Sh.html
