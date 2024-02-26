---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:37.811429-07:00
description: "Lua\u3067JSON\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\
  JSON\u5F62\u5F0F\u306E\u6587\u5B57\u5217\u3092Lua\u306E\u30C6\u30FC\u30D6\u30EB\u306B\
  \u89E3\u6790\u3057\u3001\u305D\u306E\u9006\u3082\u540C\u69D8\u306B\u884C\u3044\u3001\
  Lua\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3068Web\u30B5\u30FC\u30D3\u30B9\
  \u3084\u5916\u90E8API\u3068\u306E\u9593\u3067\u5BB9\u6613\u306B\u30C7\u30FC\u30BF\
  \u4EA4\u63DB\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001JSON\u306E\u8EFD\u91CF\u3067\u89E3\u6790\u3057\u3084\u3059\
  \u3044\u5F62\u5F0F\u3092\u5229\u7528\u3057\u3066\u3001\u30C7\u30FC\u30BF\u306E\u52B9\
  \u7387\u7684\u306A\u4FDD\u5B58\u3001\u8A2D\u5B9A\u3001\u307E\u305F\u306FAPI\u901A\
  \u4FE1\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.322825-07:00'
model: gpt-4-0125-preview
summary: "Lua\u3067JSON\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\u3001\
  JSON\u5F62\u5F0F\u306E\u6587\u5B57\u5217\u3092Lua\u306E\u30C6\u30FC\u30D6\u30EB\u306B\
  \u89E3\u6790\u3057\u3001\u305D\u306E\u9006\u3082\u540C\u69D8\u306B\u884C\u3044\u3001\
  Lua\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3068Web\u30B5\u30FC\u30D3\u30B9\
  \u3084\u5916\u90E8API\u3068\u306E\u9593\u3067\u5BB9\u6613\u306B\u30C7\u30FC\u30BF\
  \u4EA4\u63DB\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001JSON\u306E\u8EFD\u91CF\u3067\u89E3\u6790\u3057\u3084\u3059\
  \u3044\u5F62\u5F0F\u3092\u5229\u7528\u3057\u3066\u3001\u30C7\u30FC\u30BF\u306E\u52B9\
  \u7387\u7684\u306A\u4FDD\u5B58\u3001\u8A2D\u5B9A\u3001\u307E\u305F\u306FAPI\u901A\
  \u4FE1\u3092\u884C\u3044\u307E\u3059\u3002"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ?
LuaでJSONを扱うということは、JSON形式の文字列をLuaのテーブルに解析し、その逆も同様に行い、LuaアプリケーションとWebサービスや外部APIとの間で容易にデータ交換を可能にします。プログラマーは、JSONの軽量で解析しやすい形式を利用して、データの効率的な保存、設定、またはAPI通信を行います。

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
