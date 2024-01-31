---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JSONはデータのやり取りフォーマット。簡単で、多くの言語に対応しているから、プログラマはよく使う。

## How to: (やり方)
```Lua
-- JSON ライブラリの読み込み
local json = require("json")

-- JSON 文字列
local jsonString = '{"name": "Taro", "age": 30}'

-- JSON のパース
local person = json.decode(jsonString)

-- データの使用
print(person.name)  -- 出力: Taro

-- Lua テーブルから JSON 文字列へ
local table = {name = "Hanako", age = 25}
local toJson = json.encode(table)

print(toJson)  -- 出力: {"name":"Hanako","age":25}
```

## Deep Dive (深掘り)
JSONはJavaScript Object Notationの略。JavaScriptに由来しているが、独立したデータフォーマットとして成立。XMLは別の代替手段だが、JSONの方が軽量。Luaでは、dkjsonやcjsonなどのライブラリを使って実装されることが多い。

## See Also (関連情報)
- Luaの公式サイト: [http://www.lua.org/](http://www.lua.org/)
- JSONについてのより詳しい情報: [https://www.json.org/json-ja.html](https://www.json.org/json-ja.html)
- dkjsonライブラリ: [https://luarocks.org/modules/luarocks/dkjson](https://luarocks.org/modules/luarocks/dkjson)
- cjsonライブラリ: [https://luarocks.org/modules/openresty/lua-cjson](https://luarocks.org/modules/openresty/lua-cjson)
