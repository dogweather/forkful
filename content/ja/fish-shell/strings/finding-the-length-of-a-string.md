---
title:                "文字列の長さを求める"
aliases:
- /ja/fish-shell/finding-the-length-of-a-string/
date:                  2024-01-20T17:47:22.198873-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
文字列の長さを調べるのは、その文字数を数えることです。プログラマーはデータ処理や入力の検証にこの情報を使用します。

## How to: (やり方)
```Fish Shell
# 文字列の長さを取得する
set string "こんにちは"
echo (string length $string)
```
```
5
```

## Deep Dive (深堀り)
Fish Shellの `string length` コマンドは文字列の文字数を返す。これはUnicode文字を正しく数える。以前では、文字列の長さを数えるのは `wc -m` などの外部コマンドに依存していたが、内部コマンドを使うことで速度が向上する。他のシェルスクリプト言語では、例えば Bash では `${#string}` 構文を使うが、Fishはよりシンプルで直感的な `string length` を提供する。

## See Also (参照)
- Fish documentation on string manipulation: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Fish tutorial for beginners: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- Unicode Standard: [https://unicode.org/standard/standard.html](https://unicode.org/standard/standard.html)
