---
title:                "文字列の大文字化"
html_title:           "Lua: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

大文字変換は、文字列のすべての文字を大文字に変換する手法です。プログラマーは、文字列の一致比較を達成するため、またはUI表示上のニーズに応じてこれを行います。

## 実践方法

Luaでは、大文字に変換するには `string.upper` 関数を使用します。

```Lua
local str = 'hello, world'
str = string.upper(str)
print(str)
```

結果出力: 
```
HELLO, WORLD
```

## ディープダイブ

Luaでは文字列を管理するための豊富な関数群が提供されています。`string.upper` 関数はこれらの一部で、Lua 5.0から利用可能となりました。

大文字変換の代替手段としては、自分でループを作成してそれぞれの文字を大文字に変換するという方法がありますが、これは`string.upper` を使うよりもコードが長くなります。また、Luaの内部での大文字変換の具体的な実装については、それぞれの文字をその大文字版にマッピングするためのテーブルを使用しています。

## 関連情報

- [Lua公式ドキュメンテーション](https://www.lua.org/manual/5.4/)
- [Luaの文字列関数について（Programming in Lua）](https://www.lua.org/pil/20.html)