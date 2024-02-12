---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- /ja/lua/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:46.693435-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPLはRead-Eval-Print Loopの略で、コードを迅速にテストできるインタラクティブな環境です。プログラマーは、実験、デバッグ、言語の特性を学ぶためにこれを使用します。

## どのように：
LuaのREPLに入るには、端末で`lua`と入力するだけです。以下はセッションの例です：

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1 apple
2 banana
3 cherry
4 date
>
```
このセッションでは、変数を宣言し、基本的な算術を実行し、テーブルを操作し、その項目をループ処理します。

## ディープダイブ
Luaの軽量さは、プロトタイピングに理想的なREPLを作ります。これは1990年代初頭のLuaの誕生以来あり、Lispのような言語の以前のインタラクティブシェルに触発されました。他の言語での代替手段には、Rubyの`irb`やPythonの`python`などがあり、それぞれ独自の機能セットがあります。LuaのREPLは最小主義的であるため、複雑なデバッグツールのような他のものにある高度な機能が欠けている場合があります。より充実した体験を求める場合、ZeroBrane StudioやLuaDistのLuaRocksのようなツールは、基本的なREPL以上のものを提供します。

## 参照
- [Lua 5.4 リファレンスマニュアル - スタンドアロンLuaインタープリタ](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
