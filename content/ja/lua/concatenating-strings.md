---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結は、ふたつ以上の文字列を一つに結合するプログラミング用語です。プログラマーはこれを行うことにより、複雑なメッセージを構築したり、データを整形したりします。

## 方法: 

Luaでは、`..` 演算子を使って文字列を連結できます。

```Lua
string1 = "こんにちは、"
string2 = "世界"
combo = string1 .. string2   
print(combo)
```

このコードを実行すると、「こんにちは、世界」が出力されます。

## ディープダイブ:

文字列の連結はプログラミングの基本的な特性であり、Luaでは非常にシンプルです。この機能はLua 1.0から存在しており、その効率性と欠点を理解することはLuaプログラミングの重要な部分です。

代替手段としては、`string.format` 関数を使う方法があります。これは特にあるパターンに従って文字列を整形したい場合に便利です。

```Lua
weather = "晴れ"
temp = 23
message = string.format("今日の天気は%sで、気温は%d度です。", weather, temp)
print(message)
```

実装の詳細については、ほとんどの場合 `..` 演算子は十分に速く動作しますが、大量の文字列を連結する必要がある場合は、パフォーマンスを向上させるために `table.concat` 関数を使用することを検討してみてください。

## 関連リンク:

1. [Luaの公式ドキュメンテーション](https://www.lua.org/manual/5.4/)
2. [Luaの文字列連結のWikiブック](https://en.wikibooks.org/wiki/Lua_Programming/string_concatenation)
3. [StackOverflow上の文字列連結ディスカッション](https://stackoverflow.com/questions/987772/lua-string-concatenation-performance)