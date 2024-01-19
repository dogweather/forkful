---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Elixir: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何と何故？

ディレクトリが存在するか確認するとは、その指定したディレクトリパスが実際に存在するのか何か処理を実行する前に確認することです。この確認フローは、エラーや予期しない結果を防ぐためにプログラマーによって行われます。

## 手順：

Luaでは、以下のコードを用いて目的のディレクトリが存在するか、またそれがディレクトリであるかどうかを確認することができます。

```Lua
local lfs = require('lfs')

local function dirExists(dirname)
    if lfs.attributes(dirname, "mode") == "directory" then
        return true
    else
        return false
    end
end

--サンプル出力
print(dirExists('/path/to/directory'))  --存在すれば true、存在しなければ false を返す
```

## ディープダイブ：

過去のバージョンのLuaでは`lfs`のライブラリがなかったため、ディレクトリの存在確認は通常、`os.execute`を使ってシステムコマンドを実行することによって行われていました。

また、`lfs`ライブラリ以外にも、`io.popen`を使う方法が代替手段として存在します。

その実装方法は以下の通りです。

```Lua
local function dirExists(dirname)
    local f = io.popen("if [ -d " .. dirname .. " ]; then echo true; else echo false; fi")
    local exists = f:read("*a")
    f:close()
    return exists == "true\n"
end
```

だが、この方法はシェルに依存しており、ポータブルではありません。そのため、市場に出るソフトウェアを開発する場合、`lfs`ライブラリを使用することが推奨されます。

## 関連情報：

以下のリンクには、Luaでのディレクトリ存在確認に関する役立つ情報があります。
1. LuaFileSystem（lfs）公式ドキュメンテーション：https://keplerproject.github.io/luafilesystem/manual.html
2. StackOverflowのLuaカテゴリー: https://stackoverflow.com/questions/tagged/lua
3. Lua: 入門できるリソース：http://lua-users.org/wiki/TutorialDirectory