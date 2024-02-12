---
title:                "テキストの検索と置換"
aliases:
- /ja/lua/searching-and-replacing-text/
date:                  2024-01-20T17:58:10.983804-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキスト検索と置換は、文字列内で特定のパターンを見つけて、それを別のテキストで置き換えることです。プログラマーは、データ整理やコードの修正でこれをよく使います。

## How to: (実践方法)
```Lua
local text = "今日は晴れです。明日も晴れるかもしれません。"
local pattern = "晴れ"
local replacement = "雨"

-- 検索して置換
local result = text:gsub(pattern, replacement)

print(result) -- "今日は雨です。明日も雨るかもしれません。"
```

## Deep Dive (深掘り)
Luaでは`string.gsub`関数を使って簡単にテキスト検索と置換ができます。1993年の登場以来、Luaは拡張性とポータビリティが重視されており、様々な環境で使われています。`gsub`はグローバル置換の略で、パターンマッチングを優れた機能として提供していますが、正規表現は完全にはサポートしていません。代わりに、Luaのパターンマッチングは限定的ですが、多くの一般的なユースケースには十分です。正規表現が必要な場合、外部ライブラリーを利用することができます。

## See Also (関連情報)
- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
