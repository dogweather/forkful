---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列の長さを探すとは、文字列の中にある文字の数を数えることです。プログラマーがそれを行う理由は、データ操作を制御し、エラーを回避するためです。

## 方法：
```Fish Shell
# 文字列の定義
set str 'こんにちは、世界'

# 文字列長の計算
echo (string length $str)
```
出力結果：`10`

## ディープダイブ
歴史的な文脈で見ると、文字列の長さを見つける操作は古くからあります。他にも方法はある（例えばバイト単位での長さ計測）が、文字単位での長さ計測は最も一般的です。この実装において、Fish Shellは内部で文字列を配列として扱い、その要素数を数えることで文字列の長さを見つけます。

## 参考情報
- Fish Shellの公式ドキュメント : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `string length`についての詳細な説明 : [https://fishshell.com/docs/current/cmds/string-length.html](https://fishshell.com/docs/current/cmds/string-length.html)