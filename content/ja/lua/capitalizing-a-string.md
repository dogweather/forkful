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

文字列の大文字化とは、文字列の全ての文字を大文字に変換することです。プログラマーがこれを行うのは、データを統一的に扱いやすくするためです。

## 方法：

```Lua
-- 文字列を大文字に変換する例
my_string = "hello world"
print(my_string:upper())
-- 出力: "HELLO WORLD"
```

```Lua
-- 大文字化した文字列を変数に代入する例
my_string = "hello world"
uppercase_string = my_string:upper()
print(uppercase_string)
-- 出力: "HELLO WORLD"
```

## 詳細：

(1) 文字列の大文字化は、プログラミング言語によって異なる方法で実装されています。また、一部の言語ではアルファベット以外の文字も大文字になる場合があります。

(2) 文字列の大文字化には、様々な代替手段があります。例えば、文字列全体をループで処理する方法や、文字コードを変更する方法などがあります。

(3) Luaでは、文字列の大文字化には "string.upper()" という組み込み関数を使用します。これにより、文字列を直接大文字化することができます。

## 関連情報：

- [Lua公式ドキュメント](https://www.lua.org/manual/5.4/#6.4.2) - 文字列を扱う方法が記載されています。
- [String manipulation in Lua](http://lua-users.org/wiki/StringManipulation) - Luaでの文字列操作のさまざまな方法が紹介されています。