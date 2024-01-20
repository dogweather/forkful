---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

正規表現はテキスト検索・置換に使います。パターンを定義して、大量のデータから必要な情報をササッと探すためにプログラマーは使います。

## How to: (使い方)

grepの例:
```Bash
echo "ハローワールド123" | grep -oE '[0-9]+'  # 数字を検索
```
出力: `123`

sedで置換:
```Bash
echo "ハローワールド123" | sed -E 's/[0-9]+/456/'  # 数字を456に置換
```
出力: `ハローワールド456`

## Deep Dive (深掘り)

正規表現は1960年代に登場。他にも検索・置換にはawkやperlがあります。Bashではgrep, sed, awkなど複数のツールで正規表現が使え、それぞれにパフォーマンスや機能の差があります。

## See Also (関連情報)

- grep マニュアル: https://www.gnu.org/software/grep/manual/grep.html
- sed マニュアル: https://www.gnu.org/software/sed/manual/sed.html
- 正規表現について深く学ぶ: https://www.regular-expressions.info/tutorial.html