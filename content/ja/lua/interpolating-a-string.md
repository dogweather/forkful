---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

文字列補間は、文字列内で変数の値を直接使うテクニックのことです。これはコードを読みやすく短くし、エラーの可能性を減らすためにプログラマーが行います。

## どうやるのか：

以下はLuaでの文字列補間の基本的な書き方とサンプル出力です：

```Lua
name = "Tanaka"
greeting = ("Hello, %s"):format(name)
print(greeting)
```

このコードの出力は以下の通りです：

```Lua
Hello, Tanaka
```

この例では、`%s`が後で`name`変数に置き換えられます。

## より詳しく：

1. **歴史的背景**:
   Luaはシンプルさと拡張性を重視した設計です。そのため、文字列補間は他の言語ほどフレキシブルではありませんが、必要な情報は提供します。

2.. **代替手段**:
   Luaではstring.format関数を使用して文字列補間を行いますが、Lua 5.3以降では新たに導入されたstring.interp関数も使用できます。

```Lua
name = "Tanaka"
greeting = string.interp("Hello, {name}", {name = name})
print(greeting)
```

このコードの出力は以下の通りです：

```Lua
Hello, Tanaka
```

3. **実装詳細**:
   文字列補間は、内部的には指定された書式指定子に対応する引数を取り、それを文字列に変換して結果の文字列を生成します。

## 参考までに：

Luaでの文字列補間について更に学ぶには以下のリンクを確認してみてください：

- 一般的な情報： Lua-Users Wiki（http://lua-users.org/wiki/StringInterpolation）
- string.formatの詳細： Lua 5.4 Reference Manual（https://www.lua.org/manual/5.4/manual.html#pdf-string.format）
- string.interpの詳細： Lua 5.3 Reference Manual（https://www.lua.org/manual/5.3/manual.html#pdf-string.interp）