---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"

category:             "Fish Shell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を大文字に変換するとは、全ての文字を大文字の形に変えることです。プログラマーは一貫性、強調、またはフォーマット要件のためにこれを行います。

## How to: (やり方)
```Fish Shell
function capitalize
    echo $argv | tr '[:lower:]' '[:upper:]'
end

echo "konichiwa world" | capitalize # KONICHIWA WORLD
```

## Deep Dive (深掘り)
Fish Shellでの大文字変換は単純。`tr`は古くからあるUNIXコマンドで、文字を置換します。`[:lower:]`と`[:upper:]`はそれぞれ小文字と大文字の範囲です。代わりに`string to-upper`ビルトイン関数も使えますが、`tr`はよりベーシックなオプションです。Fishは3.x バージョン以降で使われており、スクリプティングがよりシンプルになっています。

## See Also (関連情報)
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [Unix `tr` Command](https://man7.org/linux/man-pages/man1/tr.1.html)
- [Shell Scripting Tutorial](https://www.shellscript.sh)
