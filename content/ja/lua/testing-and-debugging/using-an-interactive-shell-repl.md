---
date: 2024-01-26 04:16:46.693435-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Lua\u306EREPL\u306B\u5165\u308B\
  \u306B\u306F\u3001\u7AEF\u672B\u3067`lua`\u3068\u5165\u529B\u3059\u308B\u3060\u3051\
  \u3067\u3059\u3002\u4EE5\u4E0B\u306F\u30BB\u30C3\u30B7\u30E7\u30F3\u306E\u4F8B\u3067\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.310249-06:00'
model: gpt-4-0125-preview
summary: "Lua\u306EREPL\u306B\u5165\u308B\u306B\u306F\u3001\u7AEF\u672B\u3067`lua`\u3068\
  \u5165\u529B\u3059\u308B\u3060\u3051\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u30BB\u30C3\
  \u30B7\u30E7\u30F3\u306E\u4F8B\u3067\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
