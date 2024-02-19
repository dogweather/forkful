---
aliases:
- /ja/lua/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:59.237664-07:00
description: "\u2026"
lastmod: 2024-02-18 23:08:55.046957
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認することは、ファイルシステムとやり取りするスクリプトを書くときの基本的な操作であり、プログラムが有効なパスで操作し、存在しないディレクトリに関連するエラーを防ぐことを保証します。新しいファイルをディレクトリに作成したり、それらから読み取ったり、ディレクトリ固有の操作を安全に実行したりするために、このタスクは重要です。

## 方法：

Luaでは、ディレクトリが存在するかどうかを直接確認するための組み込み関数がないため、通常はLuaファイルシステム（lfs）ライブラリ、人気のあるサードパーティのファイル操作ライブラリを使用します。

まず、Luaファイルシステムがインストールされていることを確認します。そうでない場合は、通常LuaRocksを使用してインストールできます：

```sh
luarocks install luafilesystem
```

次に、以下の例を使用してディレクトリの存在を確認します：

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- 特定のディレクトリが存在するか確認
if directoryExists("/path/to/your/directory") then
    print("ディレクトリは存在します。")
else
    print("ディレクトリは存在しません。")
end
```

これにより、出力されます：

```
ディレクトリは存在します。
```

もしくは、ディレクトリが存在しない場合：

```
ディレクトリは存在しません。
```

このアプローチは、`lfs.attributes`関数を使用してパスの属性を取得します。パスが存在し、その`mode`属性が`directory`である場合、ディレクトリの存在を確認します。
