---
title:                "テキストファイルの作成"
html_title:           "Lua: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何として & なぜ？
テキストファイルを書くとは、テキストデータをコンピュータ内のファイルに保存することです。プログラマーは、データの永続性を確保したり、プログラムの出力を保存したりするために、テキストファイルを書きます。

## 方法：
```Lua
-- ファイルを作成し、データを書き込む
local file = io.open("sample.txt", "w")
file:write("Hello, world!")
file:close()

-- ファイルを読み込んでデータを出力する
local file = io.open("sample.txt", "r")
print(file:read("*a"))
file:close()
```

## 深掘り：
テキストファイルを書くことは、古くから行われてきました。コンピュータが普及する前は、テキストファイルを書くことは手書きで書類を作成するようなものでした。代替として、データベースやスプレッドシートなどのより高度な管理ツールがあります。テキストファイルの書き込みには、ほとんどのプログラミング言語でサポートされている標準の関数が利用できます。

## 関連情報：
- [Lua公式ドキュメント：ファイル操作](https://www.lua.org/pil/21.1.html)