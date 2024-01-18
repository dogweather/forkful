---
title:                "サブストリングの抽出"
html_title:           "Lua: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何が？なぜ？

文字列から部分文字列を取り出すことを抽出すると、プログラマーは特定の文字列を取得したり、正規表現によるパターンマッチングを可能にしたりすることができます。

## 方法：

	```Lua
	-- 文字列 "Hello, Lua" から "Lua" を抽出する
	local str = "Hello, Lua"
	local substr = string.sub(str, 8) -- "Lua"を抽出
	print(substr) -- 出力は "Lua" になる
	```

	```Lua
	-- 文字列 "I love coding" から "coding"を抽出する
	local str = "I love coding"
	local substr = string.sub(str, 8, 13) -- "coding"を抽出
	print(substr) -- 出力は "coding" になる
	```

## 深堀り：

抽出する方法は多くありますが、Luaでは基本的には`string.sub()`関数を使用します。また、パターンマッチングのために`string.find()`や`string.match()`を使用することもできます。

## 関連情報：

- [Luaの公式ドキュメント](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [W3SchoolsのLuaチュートリアル](https://www.w3schools.com/lua/)