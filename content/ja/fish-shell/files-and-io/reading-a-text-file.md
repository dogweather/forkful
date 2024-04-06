---
date: 2024-01-20 17:54:05.279920-07:00
description: "How to (\u3084\u308A\u65B9): \u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\
  \u30EB\u3092\u8AAD\u3080\u3053\u3068\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u306E\u53E4\u5178\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002UNIX\u7CFB\u30B7\
  \u30B9\u30C6\u30E0\u3067\u306F\u3001'70\u5E74\u4EE3\u304B\u3089`cat`, `more`, `less`\u306A\
  \u3069\u306E\u30B3\u30DE\u30F3\u30C9\u3067\u884C\u308F\u308C\u3066\u3044\u307E\u3059\
  \u3002Fish Shell\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.623979-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u53E4\u5178\u7684\u306A\
  \u30BF\u30B9\u30AF\u3067\u3059\u3002UNIX\u7CFB\u30B7\u30B9\u30C6\u30E0\u3067\u306F\
  \u3001'70\u5E74\u4EE3\u304B\u3089`cat`, `more`, `less`\u306A\u3069\u306E\u30B3\u30DE\
  \u30F3\u30C9\u3067\u884C\u308F\u308C\u3066\u3044\u307E\u3059\u3002Fish Shell \u306F\
  \u3053\u308C\u3089\u306E\u30B3\u30DE\u30F3\u30C9\u3092\u5229\u7528\u3057\u307E\u3059\
  \u304C\u3001\u64CD\u4F5C\u3092\u3088\u308A\u76F4\u611F\u7684\u306B\u3057\u3066\u3044\
  \u307E\u3059\u3002\u30D0\u30A4\u30CA\u30EA\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\
  \u4EE3\u308F\u308A\u306B\u3001`xxd` \u3084 `hexdump` \u306E\u3088\u3046\u306A\u30C4\
  \u30FC\u30EB\u304C\u3042\u308A\u307E\u3059\u3002Fish \u306E `read` \u30B3\u30DE\u30F3\
  \u30C9\u306F\u30B7\u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u5185\u3067\u30D5\u30A1\
  \u30A4\u30EB\u306E\u5185\u5BB9\u3092\u884C\u3054\u3068\u306B\u8AAD\u307F\u53D6\u308A\
  \u3001\u5909\u6570\u306B\u4EE3\u5165\u3057\u3084\u3059\u304F\u3057\u307E\u3059\u3002"
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
