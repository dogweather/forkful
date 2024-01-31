---
title:                "デバッガーの使い方"
date:                  2024-01-26T03:50:27.824306-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーはプログラムの実行を検査し制御するツールで、問題が生じる箇所を容易に特定できるようにします。プログラマーはデバッガーを使用してバグを潰し、コードフローを理解し、彼らのコードがクリーンであることを確認します。

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
