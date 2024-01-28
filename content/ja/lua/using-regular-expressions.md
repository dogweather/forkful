---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
正規表現は文字列パターンを認識するための強力なツールです。プログラマーはこれを使用して、検索、置換、データ検証などを高速で正確に行います。

## How to: (方法)
Lua で正規表現のような機能を使うには、パターンマッチング機能を用います。以下に例を示します。

```Lua
local text = "私の電話番号は123-4567です。"
local pattern = "%d%d%d%-%d%d%d%d"
print(string.match(text, pattern))
```

出力:
```
123-4567
```

文字列中の単語を探すには:

```Lua
local text = "Luaは素晴らしいです。"
local pattern = "%a+"
for word in string.gmatch(text, pattern) do
    print(word)
end
```

出力:
```
Lua
は
素晴らしい
です
```

## Deep Dive (詳細情報)
正規表現は1970年代に発明されました。Luaには、PerlやPythonの正規表現と同じくらい強力ではない、単純なパターンマッチング機能が備わっています。組み込みのパターンマッチングは限られていますが、読みやすく、理解しやすいです。 `string.find`, `string.match`, `string.gmatch` の Lua 標準関数を用いてパターンマッチングが実行できます。外部ライブラリを使えば、完全な正規表現機能をLuaに追加することも可能です。

## See Also (関連情報)
- Luaマニュアル (パターンマッチングについて): [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- 正規表現に関してより広範な情報: [Regular-Expressions.info](https://www.regular-expressions.info/)
