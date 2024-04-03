---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:26.587341-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.313069-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u30C6\
  \u30B9\u30C8\u306E\u8A18\u8FF0\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u306E\u7570\u306A\u308B\u90E8\u5206\u304C\u671F\u5F85\u3069\u304A\u308A\u306B\
  \u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u81EA\u52D5\u7684\u306B\u78BA\u8A8D\u3059\
  \u308B\u305F\u3081\u306B\u3001\u5C0F\u3055\u306A\u500B\u5225\u306E\u30B3\u30FC\u30C9\
  \u7247\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\u542B\u3093\u3067\u3044\u307E\
  \u3059\u3002Lua\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3068\u3063\u3066\u3001\
  \u30C6\u30B9\u30C8\u306F\u4FE1\u983C\u6027\u3092\u78BA\u4FDD\u3057\u3001\u30B3\u30FC\
  \u30C9\u54C1\u8CEA\u306E\u7DAD\u6301\u306B\u5F79\u7ACB\u3061\u3001\u30C7\u30D0\u30C3\
  \u30B0\u30D7\u30ED\u30BB\u30B9\u3092\u52A0\u901F\u3057\u3001\u30B3\u30FC\u30C9\u30D9\
  \u30FC\u30B9\u306E\u5909\u66F4\u3092\u3088\u308A\u5B89\u5168\u306B\u884C\u3046\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## どのように：
軽量でありながら強力なスクリプト言語であるLuaは、組み込みのテストフレームワークを含まない。しかし、BustedやLuaUnitのようなサードパーティのライブラリを使用すると、テストが比較的簡単になります。ここでは、これら両方を使用した例を見ていきます。

### Bustedを使う
Bustedは、柔軟な方法でテストを書くことができる人気のLuaテストフレームワークです。まず、LuaRocks（Luaのパッケージマネージャー）を使って`luarocks install busted`でBustedをインストールします。インストールしたら、テストを書くことができます。2つの数値を合計する関数`add`のためのシンプルなテストは以下のとおりです：

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Add function", function()
  it("should add two numbers correctly", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

テストを実行するには、ターミナルで`busted`を実行します。テストが通った場合のサンプル出力は次のようになります：

```
●
1 success / 0 failures / 0 errors / 0 pending : 0.002 seconds
```

### LuaUnitを使う
LuaUnitはxUnit規約に従う別のテストフレームワークで、セットアップが簡単です。LuaRocksを使って`luarocks install luaunit`でLuaUnitをインストールします。上記と同様のテストをLuaUnitで書く方法は以下のとおりです：

```lua
-- add.luaはそのまま

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

このスクリプトをLuaで直接実行する（`lua test_add.lua`）と、次のような出力が得られます：

```
.
Ran 1 tests in 0.001 seconds, 1 success, 0 failures
```

BustedとLuaUnitの両方とも、モッキング、スパイ、非同期テストを含む様々なテストシナリオを処理するための広範囲の機能を提供しています。それらの選択は、プロジェクトの具体的なニーズと、構文および機能に関する個人的な好みによります。
