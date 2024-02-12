---
title:                "テキストの検索と置換"
aliases:
- /ja/fish-shell/searching-and-replacing-text/
date:                  2024-01-20T17:57:59.278779-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

テキスト検索と置換は、ある文字列を見つけて他の文字列に変える操作です。プログラマはコードの修正、データ整形、自動化処理でこれをよく使います。

## How to: (方法)

```Fish Shell
# 文字列 'fish' を 'shark' に置換する
echo "I love fish tacos" | string replace "fish" "shark"
# 出力: I love shark tacos

# ファイル内の全 'fish' を 'shark' に置換
string replace -a -i "fish" "shark" file.txt
# file.txt 内の全ての 'fish' が 'shark' に置換される
```

## Deep Dive (深い潜水)

Fish Shellでは`string`ツールが文字列操作のために用意されています。古いシェルでは`sed`や`awk`が主流でしたが、Fishはより直観的に使えるコマンドを提供します。例えば、`string replace`は直接的な命名で何をするか明白です。実装面では、FishはUTF-8エンコーディングの文字列に対応し、設計が単純でわかりやすいです。

## See Also (関連情報)

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [GNU Sed Manual](https://www.gnu.org/software/sed/manual/sed.html) - 別の検索・置換ツール
- [AWK Programming Language](https://www.gnu.org/software/gawk/manual/gawk.html) - テキスト処理のためのプログラム言語
