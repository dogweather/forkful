---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:48:01.384118-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを測るとは、文字列に含まれる文字の数を数えることです。これを行う理由は、入力検証、データ処理、あるいは単純に表示目的など様々です。

## How to: (方法)
```Lua
local myString = "こんにちは"
local length = #myString
print(length)  -- Lua 5.3以降ではこの出力は9
```
サンプル出力：
```
9
```

```Lua
-- 別の方法
local myString = "こんにちは"
local length = string.len(myString)
print(length)  -- 出力は9
```
サンプル出力：
```
9
```

## Deep Dive (深堀り)
Luaでは、文字列の長さはシャープ（`#`）記号で簡単に求めることができます。古いバージョンでは`string.len`関数もよく使われていましたが、今では`#`の方が一般的です。

文字列の長さを測るための実装は、Luaのバージョンにより異なることがあります。特に、Lua 5.3以降では、UTF-8エンコーディングされた文字列が正しく扱われるようになりました。この変更により、多言語サポートが改善されています。

ですが、注意が必要です。`#`はバイト単位での長さを返すため、マルチバイト文字を含む文字列では意図しない結果となることがあります。例えば、上記の例では"こんにちは"は5文字ですが、UTF-8では日本語の文字が3バイトでエンコードされるため、9という長さが返されます。

代わりに、特定の文字エンコーディングで文字列の長さを正確に知りたい場合はライブラリを使う方法もあります。例えば、Luaの`utf8.len`関数はUTF-8エンコードされた文字列の実際の文字数を返しますが、これはLua 5.3で導入された標準ライブラリです。

## See Also (参考情報)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Programming in Lua (first edition)](https://www.lua.org/pil/contents.html)
- [`utf8.len` in the Lua 5.3 Reference Manual](https://www.lua.org/manual/5.3/manual.html#6.5)
