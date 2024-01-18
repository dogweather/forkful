---
title:                "文字列を小文字に変換する"
html_title:           "Lua: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換することは、プログラマーにとって非常に便利です。小文字の文字列を使用することで、検索や比較が容易になります。また、データの整理や処理を行う際にも、小文字に統一することで処理がスムーズになります。

## 方法：

### 例1：文字列の小文字に変換する

```
Lua
local str = "HELLO WORLD!"
print(str:lower())  --> hello world!
```

### 例2：変数の値を小文字に変換する

```
Lua
local name = "JESSICA"
print("Hello, " .. name:lower())  --> Hello, jessica
```

## 深く掘り下げる

### 歴史的背景：

文字列の小文字変換は、古くから使われているテキスト処理の基本的な技術の1つです。プログラミング言語やツールによって、実装方法が異なる場合がありますが、目的は同じです。

### 代替案：

一部のプログラミング言語では、小文字の文字列を使用するために、比較や検索を行う前に、文字列を手動で小文字に変換する必要があります。しかし、Luaは文字列オブジェクトにメソッドを提供しているため、より簡単に小文字への変換が可能です。

### 実装の詳細：

Luaでは、文字列オブジェクトに定義されたメソッド```lower()```を使用して、文字列を小文字に変換できます。このメソッドは、文字列内のすべての文字を小文字に変換します。

## 関連リンク：

- [Lua: Official Website](https://www.lua.org/)
- [Lua: String Library](https://www.lua.org/manual/5.3/manual.html#6.4)
- [Lua: String Methods](https://www.lua.org/manual/5.3/manual.html#6.4.1)