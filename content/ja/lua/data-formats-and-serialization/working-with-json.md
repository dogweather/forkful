---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:37.811429-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066? Lua\u306B\u306FJSON\u51E6\
  \u7406\u7528\u306E\u7D44\u307F\u8FBC\u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u542B\
  \u307E\u308C\u3066\u3044\u307E\u305B\u3093\u3002\u305D\u306E\u305F\u3081\u3001`dkjson`\u306E\
  \u3088\u3046\u306A\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u88FD\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u7528\u3044\u3066\u3001JSON\u306E\
  \u30A8\u30F3\u30B3\u30FC\u30C9\u3068\u30C7\u30B3\u30FC\u30C9\u3092\u7C21\u5358\u306B\
  \u884C\u3044\u307E\u3059\u3002\u307E\u305A\u3001LuaRocks\uFF08`luarocks install\u2026"
lastmod: '2024-04-05T21:53:43.177828-06:00'
model: gpt-4-0125-preview
summary: "Lua\u306B\u306FJSON\u51E6\u7406\u7528\u306E\u7D44\u307F\u8FBC\u307F\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\u3002\
  \u305D\u306E\u305F\u3081\u3001`dkjson`\u306E\u3088\u3046\u306A\u4EBA\u6C17\u306E\
  \u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u88FD\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u7528\u3044\u3066\u3001JSON\u306E\u30A8\u30F3\u30B3\u30FC\u30C9\u3068\
  \u30C7\u30B3\u30FC\u30C9\u3092\u7C21\u5358\u306B\u884C\u3044\u307E\u3059\u3002\u307E\
  \u305A\u3001LuaRocks\uFF08`luarocks install dkjson`\uFF09\u3092\u901A\u3058\u3066\
  `dkjson`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3059\u308B\u3053\u3068\u3092\
  \u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002\u305D\u306E\u5F8C\u3001\u4EE5\
  \u4E0B\u306E\u4F8B\u306B\u5F93\u3063\u3066\u304F\u3060\u3055\u3044\u3002"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## どのようにして?
LuaにはJSON処理用の組み込みライブラリが含まれていません。そのため、`dkjson`のような人気のあるサードパーティ製ライブラリを用いて、JSONのエンコードとデコードを簡単に行います。まず、LuaRocks（`luarocks install dkjson`）を通じて`dkjson`をインストールすることを確認してください。その後、以下の例に従ってください。

### JSONをLuaテーブルへデコード
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Luaプログラマー", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("エラー:", err)
else
  print("名前:", luaTable.name) -- 出力: 名前: Luaプログラマー
  print("年齢:", luaTable.age) -- 出力: 年齢: 30
  print("言語:", table.concat(luaTable.languages, ", ")) -- 出力: 言語: Lua, JavaScript
end
```

### LuaテーブルをJSONへエンコード
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Luaプログラマー",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

エンコーディングのサンプル出力:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Luaプログラマー"
}
```

これらの簡単な例は、Luaアプリケーションを様々なWeb技術や外部APIと簡単に統合する方法を示しています。`dkjson`をこれらの例で使用しましたが、`cjson`や`RapidJSON`のような他のライブラリも、プロジェクトのニーズに応じて適切な代替手段となることを覚えておいてください。
