---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？

部分文字列の抽出とは、より大きな文字列から特定の範囲の文字を選択して取り出す操作のことです。プログラマーがこれを行う理由は、大量のデータを操作、解析、フィルタリングするためです。

## 方法：

Luaでは、「string.sub」を使用します。次はその例です：

```Lua
str = "こんにちは、世界！"
print(string.sub(str, 3, 5))  -- "にち"
```
このプログラムを実行すると、「にち」と出力されます。2番目の引数は始点、3番目の引数は終点の位置を指定します。

## より深く：

Luaの「string.sub」関数は演算子が1から始まることを前提としています。これは言語の設計の一部で、他の言語とは異なります。代替として、「string.slice」のような他の文字列操作関数も利用できますが、Luaでは「string.sub」が最も一般的です。また、「string.sub」は非常に効率的に実装されており、大量の文字列を処理するときには理想的です。

## 参考情報：

それぞれの使用方法や実装詳細については以下を参照してください：

1. 公式Luaマニュアル: [string.sub](https://www.lua.org/manual/5.4/manual.html#6.4.2)
2. Lua Users Wiki: [Strings Tutorial](http://lua-users.org/wiki/StringsTutorial)
3. Stack Overflow: [How do I get a substring in Lua?](https://stackoverflow.com/questions/19664666/how-do-i-get-a-substring-in-lua)