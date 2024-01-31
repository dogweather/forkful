---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:57:49.664638-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかをチェックすることは、ファイルシステムにおけるディレクトリの存在を確認する処理です。これは、データを保存したり、読み込んだりする前に、エラーを防ぎ、プログラムがスムーズに動作するために行われます。

## How to: (方法)
```Lua
local lfs = require("lfs")

function directory_exists(path)
    local attributes = lfs.attributes(path)
    return attributes and attributes.mode == "directory"
end

-- 例:
if directory_exists("some/directory") then
    print("ディレクトリが存在します。")
else
    print("ディレクトリが存在しません。")
end
```

サンプル出力:
```
ディレクトリが存在します。
```
または
```
ディレクトリが存在しません。
```

## Deep Dive (深い潜入)
Luaには組み込みのディレクトリチェック関数がないため、LuaFileSystem（lfs）という外部ライブラリを使います。このライブラリは、Luaのファイルシステム関連の機能を拡張します。歴史的に、外部ライブラリに頼るのはLuaの一般的なパターンであり、コア言語が軽量であるためです。他の言語であれば標準ライブラリに含まれている機能も、Luaでは外部モジュールを用いる必要があります。

実装の詳細として、`lfs.attributes`関数は、与えられたパスが指すオブジェクトの属性をテーブル形式で返します。このテーブルの`mode`属性をチェックして、`"directory"`かどうかを確認します。さらに`lfs`ライブラリは多くのプラットフォームをサポートしていますが、環境によっては別の方法や追加のセットアップが必要になることもあります。

## See Also (さらに見る)
- Programming in Lua (第四版): [https://www.lua.org/pil/](https://www.lua.org/pil/)
