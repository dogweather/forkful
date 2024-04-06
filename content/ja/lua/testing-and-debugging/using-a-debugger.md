---
date: 2024-01-26 03:50:27.824306-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Lua\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\
  \u30C7\u30D0\u30C3\u30AC\u30FC\u304C\u3042\u308A\u307E\u305B\u3093\u304C\u3001ZeroBrane\
  \ Studio\u306E\u3088\u3046\u306A\u5916\u90E8\u306E\u3082\u306E\u3092\u4F7F\u7528\
  \u3067\u304D\u307E\u3059\u3002\u3053\u308C\u304C\u305D\u308C\u3092\u4F7F\u3046\u969B\
  \u306E\u4E00\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.836280-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u3044\u65B9\uFF1A Lua\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u30C7\
  \u30D0\u30C3\u30AC\u30FC\u304C\u3042\u308A\u307E\u305B\u3093\u304C\u3001ZeroBrane\
  \ Studio\u306E\u3088\u3046\u306A\u5916\u90E8\u306E\u3082\u306E\u3092\u4F7F\u7528\
  \u3067\u304D\u307E\u3059\u3002\u3053\u308C\u304C\u305D\u308C\u3092\u4F7F\u3046\u969B\
  \u306E\u4E00\u4F8B\u3067\u3059\uFF1A."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 使い方：
Luaには組み込みのデバッガーがありませんが、ZeroBrane Studioのような外部のものを使用できます。これがそれを使う際の一例です：

```Lua
-- 故意にエラーを含むシンプルなLuaスクリプト
local function add(a, b)
    local result = a + b -- おっと、「b」を定義するのを忘れたふりをしましょう
    return result
end

print(add(10))
```

これをデバッガーで実行すると、問題が生じる箇所で実行が停止します。こんな感じのことが表示されます：

```
lua: example.lua:3: nil値に対する算術操作の試み (局所変数 'b')
スタックトレースバック:
	example.lua:3: 'add' 関数内
	example.lua:7: メインチャンク内
	[C]: in ?
```

ブレークポイントを設定し、コードを一歩ずつ進め、変数の値を覗き見ることで、頭を抱えることなくバグを追跡できます。

## より深く：
残念ながら、Luaのシンプルさはデバッグには及びません。しかし心配無用です、Luaコミュニティがサポートしてくれます。ZeroBrane Studio、LuaDecなどのツールはデバッグ機能を提供しています。歴史的に、デバッガーは最初のプログラムが問題を起こした直後に存在しており、開発者が手探りでコードを修正する手段を提供していました。

Luaでは、よく外部のデバッガーに頼ったり、開発環境にそれを組み込んだりします。たとえば、ZeroBrane StudioはLuaデバッガーを完全に統合したIDEです。それによりコードを一歩ずつ実行したり、ブレークポイントを設定したり、変数を監視することができます。実装の側では、デバッガーは通常、ブレークポイントやその他のデバッグ機能を挿入するためにフックを使用します。

代替手段は？ありますよ。昔ながらの`print`ステートメント、「printfデバッグ」とも呼ばれるものは、高尚なツールなしでも時には役立つことがあります。

## 参照
デバッグの旅を続けるには、以下をチェックしてみてください：

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-users wikiのLuaコードデバッグについて: http://lua-users.org/wiki/DebuggingLuaCode
- Luaのマニュアルにある`debug`ライブラリのリファレンス：https://www.lua.org/manual/5.4/manual.html#6.10
