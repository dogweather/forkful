---
title:                "文字列を小文字に変換"
aliases: - /ja/lua/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:57.412371-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、全ての大文字を小文字にすることです。検索やソートを簡単にし、大文字小文字を問わない一貫性を保つために行われます。

## How to: (方法)
```Lua
-- 文字列を小文字に変換する例
local originalString = "Hello, World!"
local lowerCaseString = originalString:lower()

print(lowerCaseString)  -- 出力: "hello, world!"
```

## Deep Dive (深く探る)
Luaでは、文字列操作は基本的で必要な作業です。`string.lower`関数は、Lua 5.0から利用可能で、大文字から小文字への変換が簡単にできます。この関数は、ASCII文字に限定されており、ローカル言語の特殊な大文字には対応していない場合があります。代替案としては、UTF-8対応の外部ライブラリを使用することが挙げられます。内部的には、Luaの`string.lower`はCで書かれた関数を呼び出して実装されており、実行速度が速いです。

## See Also (関連情報)
- Lua 5.4リファレンスマニュアル: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
