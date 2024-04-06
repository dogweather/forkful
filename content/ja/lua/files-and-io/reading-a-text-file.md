---
date: 2024-01-20 17:54:59.835603-07:00
description: "How to / \u65B9\u6CD5 Lua\u3067\u306E\u30D5\u30A1\u30A4\u30EB\u5165\u51FA\
  \u529B\u306F\u3001\u3082\u3068\u3082\u30685.0\u304B\u3089\u306E\u6A19\u6E96\u30E9\
  \u30A4\u30D6\u30E9\u30EA`io`\u3067\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\
  \u3002`read(\"*a\")`\u3067\u306F\u30D5\u30A1\u30A4\u30EB\u5168\u4F53\u3092\u8AAD\
  \u307F\u8FBC\u307F\u307E\u3059\u3002\u4ED6\u306B\u3082`read(\"*l\")`\uFF081\u884C\
  \u8AAD\u307F\u8FBC\u307F\uFF09\u3084`read(num)`\uFF08num\u30D0\u30A4\u30C8\u8AAD\
  \u307F\u8FBC\u307F\uFF09\u304C\u3042\u308A\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.172495-06:00'
model: gpt-4-1106-preview
summary: "`io.open`\u306F\u4E8C\u3064\u306E\u5024\u3092\u8FD4\u3057\u307E\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to / 方法
```lua
-- ファイルのオープン
local file = io.open("sample.txt", "r")
-- 内容の読み取り
local content = file:read("*a")
-- 表示
print(content)
-- ファイルのクローズ
file:close()
```
出力例:
```
こんにちは、Luaの世界へようこそ！
```

エラー処理を含む安全な読み込み:
```lua
local function readFile(path)
    local file, err = io.open(path, "r")
    if not file then
        return nil, err  -- エラーを返す
    end
    local content = file:read("*a")
    file:close()
    return content, nil  -- 成功時には内容を返す
end

local content, err = readFile("sample.txt")
if err then
    print("読み込みエラー:", err)
else
    print(content)
end
```

## Deep Dive / 詳細情報
Luaでのファイル入出力は、もともと5.0からの標準ライブラリ`io`で提供されています。`read("*a")`ではファイル全体を読み込みます。他にも`read("*l")`（1行読み込み）や`read(num)`（numバイト読み込み）があります。

`io.open`は二つの値を返します: ファイルハンドルとエラーメッセージ。エラー処理は、返されるエラーメッセージで行うべきです。Luaには自動クローズはないため、`file:close()`を呼び出して、リソースリークを防ぐことが大事です。

バイナリファイルの読み込みには、テキストモード`"r"`の代わりに`"rb"`を使います。バッファリングの制御や低レベル入出力が必要なら、標準ライブラリの`io`よりも`file`モジュールを用います。

## See Also / 関連情報
- Lua公式マニュアル: [read関数](https://www.lua.org/manual/5.4/manual.html#6.8)
- エラー処理: [ピルエルア (PIL) – エラー処理と例外](http://www.lua.org/pil/8.4.html)
