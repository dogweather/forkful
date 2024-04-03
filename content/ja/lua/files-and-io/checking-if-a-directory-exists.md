---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:59.237664-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.327867-06:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\
  \u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\
  \u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u3068\u3084\u308A\u53D6\u308A\u3059\u308B\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u3092\u66F8\u304F\u3068\u304D\u306E\u57FA\u672C\u7684\u306A\
  \u64CD\u4F5C\u3067\u3042\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u6709\u52B9\
  \u306A\u30D1\u30B9\u3067\u64CD\u4F5C\u3057\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\
  \u30A3\u30EC\u30AF\u30C8\u30EA\u306B\u95A2\u9023\u3059\u308B\u30A8\u30E9\u30FC\u3092\
  \u9632\u3050\u3053\u3068\u3092\u4FDD\u8A3C\u3057\u307E\u3059\u3002\u65B0\u3057\u3044\
  \u30D5\u30A1\u30A4\u30EB\u3092\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306B\u4F5C\u6210\
  \u3057\u305F\u308A\u3001\u305D\u308C\u3089\u304B\u3089\u8AAD\u307F\u53D6\u3063\u305F\
  \u308A\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u56FA\u6709\u306E\u64CD\u4F5C\u3092\
  \u5B89\u5168\u306B\u5B9F\u884C\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\
  \u3053\u306E\u30BF\u30B9\u30AF\u306F\u91CD\u8981\u3067\u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
