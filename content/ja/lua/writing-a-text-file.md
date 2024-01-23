---
title:                "テキストファイルの書き込み"
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
テキストファイルの書き込みとは何か？それはデータや情報をテキスト形式でファイルに保存することです。プログラマーはなぜこれを行うのか？データ持続性のためと、再利用や共有が簡単になるからです。

## How to:
```Lua
-- ファイルを開く
local file = io.open("example.txt", "w")  

-- ファイルにテキストを書き込む
file:write("こんにちは、Lua！\n")  
file:write("テキストファイルのサンプルです。")

-- ファイルを閉じる
file:close()                               
```
ファイル `example.txt` には以下の内容が保存されます：
```
こんにちは、Lua！
テキストファイルのサンプルです。
```

## Deep Dive
Luaでは`io`ライブラリがファイル入出力を担当します。歴史的にみると、テキストファイルは情報交換のための基本的な方法でした。他の方法にはデータベースやネットワーク越しの通信がありますが、テキストファイルはそのシンプルさから今でもよく使われています。`io.open`関数に"w"オプションを指定すると、新しいファイルを書き込み用に開きますし、既存のファイルは上書きされます。

## See Also
- 公式Lua 5.4 マニュアル（英語）: https://www.lua.org/manual/5.4/manual.html#6.8 
- LuaのファイルI/Oチュートリアル（英語）: https://www.tutorialspoint.com/lua/lua_file_io.htm
