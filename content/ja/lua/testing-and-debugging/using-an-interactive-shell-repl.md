---
date: 2024-01-26 04:16:46.693435-07:00
description: "REPL\u306FRead-Eval-Print Loop\u306E\u7565\u3067\u3001\u30B3\u30FC\u30C9\
  \u3092\u8FC5\u901F\u306B\u30C6\u30B9\u30C8\u3067\u304D\u308B\u30A4\u30F3\u30BF\u30E9\
  \u30AF\u30C6\u30A3\u30D6\u306A\u74B0\u5883\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u8A00\u8A9E\
  \u306E\u7279\u6027\u3092\u5B66\u3076\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.310249-06:00'
model: gpt-4-0125-preview
summary: "REPL\u306FRead-Eval-Print Loop\u306E\u7565\u3067\u3001\u30B3\u30FC\u30C9\
  \u3092\u8FC5\u901F\u306B\u30C6\u30B9\u30C8\u3067\u304D\u308B\u30A4\u30F3\u30BF\u30E9\
  \u30AF\u30C6\u30A3\u30D6\u306A\u74B0\u5883\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u8A00\u8A9E\
  \u306E\u7279\u6027\u3092\u5B66\u3076\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
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
