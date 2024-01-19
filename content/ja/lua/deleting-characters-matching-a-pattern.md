---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から特定のパターンに一致する文字を削除するということは、不要な情報を取り除いて、文字列を理解しやすくするためにプログラマーがよく行う操作です。

## 使い方：

Luaでは、`gsub`関数を使用して文字列内の特定のパターンを削除します。具体的なコードは以下の通りです：

```Lua
s = "Hello, World!"
s_modified = s:gsub("World", "")
print(s_modified)
```

上記のコードを実行すると、出力結果は以下のようになります。

```Lua
Hello, !
```

## 深掘り：

`gsub`関数はLuaの歴史から存在している重要な関数です。この関数は、最初の引数に対象の文字列、二番目の引数に削除したいパターン、三番目の引数（オプション）に置換したい文字を指定します。

他の方法として、`string.gsub`または`string.match`を使用することもできますが、これらは一部のケースでは手間がかかるかもしれません。`gsub`関数が必要な操作をすぐに提供してくれます。

`gsub`関数は内部的に、与えられたパターンがマッチする部分を見つけ、それを削除（あるいは置換）するというシンプルなアルゴリズムを実行します。

## 参考資料：

以下のリンクで、Luaの公式ドキュメンテーションといくつかの関連する記事を見つけることができます:

- [Lua公式ドキュメンテーション](https://www.lua.org/manual/5.4/manual.html)
- [gsub関数の詳細（Stack Overflow）](https://stackoverflow.com/questions/6202875/gsub-to-remove-certain-patterns)
- [Luaでの文字列操作（Lua-users wiki）](http://lua-users.org/wiki/StringLibraryTutorial)