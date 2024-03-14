---
date: 2024-01-20 17:54:05.279920-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\
  \u3080\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30E1\u30E2\
  \u30EA\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u60C5\u5831\u3092\u51E6\u7406\u3001\u89E3\u6790\u3001\u307E\
  \u305F\u306F\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.760172-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\
  \u3080\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30E1\u30E2\
  \u30EA\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u60C5\u5831\u3092\u51E6\u7406\u3001\u89E3\u6790\u3001\u307E\
  \u305F\u306F\u8868\u793A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
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
