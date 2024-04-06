---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:26.587341-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A \u8EFD\u91CF\u3067\u3042\u308A\u306A\
  \u304C\u3089\u5F37\u529B\u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u8A00\u8A9E\u3067\u3042\
  \u308BLua\u306F\u3001\u7D44\u307F\u8FBC\u307F\u306E\u30C6\u30B9\u30C8\u30D5\u30EC\
  \u30FC\u30E0\u30EF\u30FC\u30AF\u3092\u542B\u307E\u306A\u3044\u3002\u3057\u304B\u3057\
  \u3001Busted\u3084LuaUnit\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3068\u3001\
  \u30C6\u30B9\u30C8\u304C\u6BD4\u8F03\u7684\u7C21\u5358\u306B\u306A\u308A\u307E\u3059\
  \u3002\u3053\u3053\u3067\u306F\u3001\u3053\u308C\u3089\u4E21\u65B9\u3092\u4F7F\u7528\
  \u3057\u305F\u4F8B\u3092\u898B\u3066\u3044\u304D\u307E\u3059\u3002\u2026"
lastmod: '2024-03-13T22:44:42.313069-06:00'
model: gpt-4-0125-preview
summary: "\u8EFD\u91CF\u3067\u3042\u308A\u306A\u304C\u3089\u5F37\u529B\u306A\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u8A00\u8A9E\u3067\u3042\u308BLua\u306F\u3001\u7D44\u307F\
  \u8FBC\u307F\u306E\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u3092\
  \u542B\u307E\u306A\u3044\u3002\u3057\u304B\u3057\u3001Busted\u3084LuaUnit\u306E\u3088\
  \u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3059\u308B\u3068\u3001\u30C6\u30B9\u30C8\u304C\u6BD4\u8F03\
  \u7684\u7C21\u5358\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\
  \u3053\u308C\u3089\u4E21\u65B9\u3092\u4F7F\u7528\u3057\u305F\u4F8B\u3092\u898B\u3066\
  \u3044\u304D\u307E\u3059."
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
