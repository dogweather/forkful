---
title:                "テストの作成"
date:                  2024-02-03T19:31:26.587341-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングにおけるテストの記述は、アプリケーションの異なる部分が期待どおりに動作することを自動的に確認するために、小さな個別のコード片を作成することを含んでいます。Luaプログラマーにとって、テストは信頼性を確保し、コード品質の維持に役立ち、デバッグプロセスを加速し、コードベースの変更をより安全に行うことができます。

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
