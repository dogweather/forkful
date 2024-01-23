---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:25.476776-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダム数を生成するのは、予測できない数を作るプロセスです。これはゲーム、シミュレーション、セキュリティなど多方面で必要とされます。

## How to: (方法)
```Lua
-- Lua 5.4でのランダム数の生成
math.randomseed(os.time()) -- 現在時刻に基づくシード値の設定
local random_number = math.random() -- ランダムな浮動小数点数（0以上、1未満）
print("Random Float: ", random_number)

local random_int = math.random(10, 100) -- 10から100までのランダムな整数
print("Random Integer: ", random_int)
```
実行するたびに、異なる数が表示されます。

## Deep Dive (深掘り)
以前のLuaバージョンでは、乱数生成器が予測可能だったため、新しいバージョンで改善されました。Luaにおける代替方法として`os.time()` 以外にも、よりセキュリティが重視される場合はエントロピーソース（例：`/dev/urandom` など）を使ったシード設定があります。Luaの`math.random`は内部でC言語の`rand`関数を利用しており、アルゴリズムは実装に依存しています。

## See Also (関連リンク)
- Lua 5.4 参考マニュアル: https://www.lua.org/manual/5.4/manual.html#6.3
- Wikipedia の乱数生成: https://ja.wikipedia.org/wiki/乱数生成器
- Online Lua Compiler (ランダム数生成器のテスト用): https://www.tutorialspoint.com/execute_lua_online.php
