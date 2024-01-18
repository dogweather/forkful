---
title:                "「文字列の補間」"
html_title:           "Lua: 「文字列の補間」"
simple_title:         "「文字列の補間」"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何やってそう? 
文字列の中に別の値を挿入することを、「文字列補間 (interpolating a string)」と呼びます。プログラマーが文字列補間をする理由は、動的な情報を含む文字列を簡単に作成するためです。

## 方法: 
文字列補間するには、```Lua ... ``` のコードブロックを使用します。以下の例を参考に、自分のコードに合わせて実装しましょう。

**シンプルな例:**
```Lua
local name = "太郎"
local greeting = "こんにちは、" .. name .. "さん！"
print(greeting)
```

出力:
``` 
こんにちは、太郎さん！
```

**数値や式の使用:**
```Lua
local x = 5
local y = 7
local equation = x .. " + " .. y .. " = " .. (x+y)
print(equation)
```

出力:
```
5 + 7 = 12
```

## 詳しく掘り下げる:
文字列補間は、通常の文字列結合 (string concatenation) と同じ原理に基づいています。実際、Luaの公式ドキュメントでは文字列補間を「文字列結合」と呼んでいます。別の方法としては文字列フォーマット (string formatting) がありますが、こちらはより複雑な書き方が必要です。Luaの文字列補間の実装では、文字列の中に```${}```で囲んだ式を書くことで、より簡単に動的な情報を挿入できるようになっています。

## さらに参考に:
- [Lua公式ドキュメント - 文字列結合 (Concatenation)](https://www.lua.org/pil/3.6.html)
- [Lua公式ドキュメント - 文字列フォーマット (Formatting)](https://www.lua.org/manual/5.3/manual.html#6.4.2)
- [文字列補間の使い方](https://qiita.com/yugoyohaze/items/9a3dec6c8213c39c4a47)