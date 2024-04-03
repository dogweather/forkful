---
date: 2024-01-20 17:54:05.279920-07:00
description: "How to (\u3084\u308A\u65B9): ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.760172-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
