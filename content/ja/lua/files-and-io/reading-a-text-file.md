---
date: 2024-01-20 17:54:59.835603-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C7\u30FC\u30BF\u3092\
  \u53D6\u5F97\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u3001\u5185\
  \u5BB9\u8868\u793A\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.317788-07:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C7\u30FC\u30BF\u3092\
  \u53D6\u5F97\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u3001\u5185\
  \u5BB9\u8868\u793A\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

テキストファイルの読み込みとは、ファイルからデータを取得するプロセスです。プログラマーは設定、データ処理、内容表示などのためにこれを行います。

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
