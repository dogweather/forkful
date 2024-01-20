---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
標準エラー出力に書き込むって何？それはプログラムからのエラーメッセージをユーザーや他のプログラムに通知する方法だ。何のためにするの？エラーと通常の出力内容を分け、エラーログを拾いやすくするためだ。

## How to: (方法)
```Lua
-- Luaで標準エラーに書き出すサンプル
io.stderr:write("エラーメッセージ\n")
```

サンプル出力:
```
エラーメッセージ
```
## Deep Dive (深掘り)
標準エラー出力はUNIX時代からある。`io.stderr`はLuaの標準ライブラリの一部で、その実装は環境によって異なりうる。例えば`print`は常に標準出力に向かうけど、`io.stderr:write`を使うと明示的にエラーストリームに出力できる。これはエラー情報のリダイレクトやログ収集に使える。

## See Also (参照)
- Luaの入出力ライブラリ: [https://www.lua.org/manual/5.4/manual.html#6.8](https://www.lua.org/manual/5.4/manual.html#6.8)
- UNIXの標準ストリームについて: [https://ja.wikipedia.org/wiki/標準ストリーム](https://ja.wikipedia.org/wiki/標準ストリーム)