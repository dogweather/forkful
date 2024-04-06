---
date: 2024-01-20 17:48:01.384118-07:00
description: "How to: (\u65B9\u6CD5) Lua\u3067\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u9577\u3055\u306F\u30B7\u30E3\u30FC\u30D7\uFF08`#`\uFF09\u8A18\u53F7\u3067\u7C21\
  \u5358\u306B\u6C42\u3081\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u53E4\
  \u3044\u30D0\u30FC\u30B8\u30E7\u30F3\u3067\u306F`string.len`\u95A2\u6570\u3082\u3088\
  \u304F\u4F7F\u308F\u308C\u3066\u3044\u307E\u3057\u305F\u304C\u3001\u4ECA\u3067\u306F\
  `#`\u306E\u65B9\u304C\u4E00\u822C\u7684\u3067\u3059\u3002 \u6587\u5B57\u5217\u306E\
  \u9577\u3055\u3092\u6E2C\u308B\u305F\u3081\u306E\u5B9F\u88C5\u306F\u3001Lua\u306E\
  \u30D0\u30FC\u30B8\u30E7\u30F3\u306B\u3088\u308A\u7570\u306A\u308B\u3053\u3068\u304C\
  \u3042\u308A\u307E\u3059\u3002\u7279\u306B\u3001Lua\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.203932-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Lua\u3067\u306F\u3001\u6587\u5B57\u5217\u306E\u9577\u3055\
  \u306F\u30B7\u30E3\u30FC\u30D7\uFF08`#`\uFF09\u8A18\u53F7\u3067\u7C21\u5358\u306B\
  \u6C42\u3081\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u53E4\u3044\u30D0\
  \u30FC\u30B8\u30E7\u30F3\u3067\u306F`string.len`\u95A2\u6570\u3082\u3088\u304F\u4F7F\
  \u308F\u308C\u3066\u3044\u307E\u3057\u305F\u304C\u3001\u4ECA\u3067\u306F`#`\u306E\
  \u65B9\u304C\u4E00\u822C\u7684\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
