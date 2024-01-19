---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを見つけるとは、その文字列がいくつの文字で構成されているかを知ることです。プログラマは、この情報を使ってデータを検証、整理、または加工することができます。

## 方法:

Luaでは、文字列の長さを計算するには「#」演算子を使用します。以下のコードはそれを示しています。

```Lua
myString = "こんにちは、世界！"
print(#myString)
```
このコードを実行すると、「21」という出力が表示されます。「こんにちは、世界！」というフレーズは実際には9文字しかありませんが、我々が扱っているのはUTF-8エンコードされた文字列であるため、このフレーズはより多くの文字を含んでいます。

## 詳細:

本来、文字列の長さを求めることはC言語に由来しています。しかしながら、Luaはこれをユーザーフレンドリーにするために「#」演算子を導入しました。また、Luaでは、UTF-8エンコードされた文字列の扱いが容易であるため、全世界の様々な言語と文字を簡単に取り扱うことができます。

代替として、「string.len()」関数も使用できますが、これは単に「#」演算子を使った方法のシンタックスシュガーです。

## 関連リンク:

- [Luaの公式ドキュメント](https://www.lua.org/manual/5.4/)
- [UTF-8についての詳細](https://en.wikipedia.org/wiki/UTF-8)
- [Luaの文字列操作のチュートリアル](https://www.tutorialspoint.com/lua/lua_strings.htm)