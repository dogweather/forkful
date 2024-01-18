---
title:                "文字列の長さを見つける"
html_title:           "Lua: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何？　なぜ？

文字列の長さを求めることは、プログラマーにとって非常に重要な作業です。文字列の長さとは、その文字列に含まれる文字の数を指します。プログラマーは、文字列の長さを求めることで、データの処理や整形に役立てることができます。

## 方法：

Luaでは非常に簡単に文字列の長さを求めることができます。下記のコードを参考にしてください。

```Lua
-- 文字列の長さを求める
local str = "こんにちは"
print(#str) -- 出力：5

-- 空の文字列の場合
local empty_str = ""
print(#empty_str) -- 出力：0
```

## 基礎知識：

文字列の長さを求めることは、データ処理や整形において重要な要素となります。古いバージョンのLuaでは、文字列の長さを求めるためには、`string.len()`関数を使用する必要がありましたが、現在では`#`演算子を用いることでより簡単に求めることができます。

## 関連情報：

- [string.len()関数のドキュメント] (https://www.lua.org/manual/5.3/manual.html#pdf-string.len)
- [文字列操作に関するトピック] (https://www.lua.org/pil/20.html)