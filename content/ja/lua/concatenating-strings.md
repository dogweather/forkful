---
title:                "文字列の連結"
aliases:
- ja/lua/concatenating-strings.md
date:                  2024-01-20T17:35:24.074177-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結は、複数の文字列を結合して一つの文字列を作ることです。プログラマーはデータの表示、ファイルパスの形成、ユーザー入力の操作などにこれを利用します。

## How to: (やり方)
```Lua
-- 文字列を連結する基本的な方法
local greeting = "こんにちは"
local name = "世界"
local message = greeting .. ", " .. name .. "!"
print(message)  -- 出力: こんにちは, 世界!
```

```Lua
-- テーブルを使って効果的に連結
local words = {"Luaは", "素晴らしい", "言語です"}
local sentence = table.concat(words, " ") -- スペースをセパレータとして使用
print(sentence)  -- 出力: Luaは 素晴らしい 言語です
```

## Deep Dive (掘り下げ)
文字列の連結はLuaの歴史を通じて基本的な機能であり、Lua 5.1からは`..`演算子を使って簡単に行われます。文字列は内部的には不変であり、新しい文字列を作るたびにメモリに新しい領域が割り当てられるため、大量の連結操作は性能に影響を与える可能性があります。そのため、`table.concat`関数は多数の文字列を連結する際の効率的な選択肢となりえます。

Luaはメタメソッド`__concat`もサポートしており、カスタムオブジェクト間の連結を定義するのに使われます。連結操作の裏で実際に行われるプロセスはLuaバージョンによって若干異なるため、Luaのドキュメントを確認することを推奨します。

## See Also (関連する情報)
- [Lua 5.4リファレンスマニュアル: 文字列](https://www.lua.org/manual/5.4/manual.html#3.4.6)
