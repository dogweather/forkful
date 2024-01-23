---
title:                "文字列の補間"
date:                  2024-01-20T17:50:18.985885-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは変数や式の結果を文字列に埋め込むことです。プログラマーはコードの可読性を上げたり、動的なメッセージを作ったりするためにこれを行います。

## How to: (実践方法)
```Bash
# 変数に値を割り当てます
greeting="こんにちは"
name="世界"

# 文字列補間を使って変数を出力します
echo "${greeting}、${name}！"

# 数式の結果も埋め込むことができます
a=5
b=10
echo "合計は: $(($a + $b))"
```
出力:
```
こんにちは、世界！
合計は: 15
```

## Deep Dive (掘り下げ)
文字列補間は古くからシェルスクリプトにおける基本機能で、効率的なコード作成に役立ちます。Bash以前は`expr`コマンドを使用しますが、今では`$()`や`${}`構文が使われています。これらは子プロセスを作らずに処理できるため効率的です。代わりにダブルクオートを使うと、変数やコマンド出力が展開されません。

## See Also (参照)
- Bash Manual: [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- Advanced Bash-Scripting Guide: [ABS Guide](https://www.tldp.org/LDP/abs/html/)
