---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)

文字列を大文字にすることは、その文字列の初めの文字を大文字に変換することです。プログラマーは可読性を高めたり、文法的な要件を満たすためによく行います。

## How to: (やり方)

```Lua
local function capitalize(str)
    return (str:gsub("^%l", string.upper))
end

print(capitalize("こんにちは")) -- 期待される出力: こんにちは
print(capitalize("luaは楽しい！")) -- 期待される出力: Luaは楽しい！
```

## Deep Dive (深掘り)

文字列の最初の文字を大文字にするのは、Luaの標準ライブラリには含まれていません。`gsub`関数を使って、最初の小文字を大文字に変換する一般的な独自の関数が必要です。PythonやJavaScriptなど他の言語には組み込みのメソッドがありますが、Luaでは関数を自作することで対応します。実装は簡単で、正規表現と`string.upper`を使い、先頭の小文字を大文字に変えることが一般的です。

## See Also (関連項目)

- Lua Users Wiki - [StringsTutorial](http://lua-users.org/wiki/StringsTutorial)
- Lua 5.4 Reference Manual - [string library](https://www.lua.org/manual/5.4/manual.html#6.4)
- Roberto Ierusalimschy's book, "Programming in Lua" - [Available online](https://www.lua.org/pil/contents.html)
