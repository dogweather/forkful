---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:17.121788-07:00
description: "YAML\u306F\"YAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:42.336229-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\"YAML Ain't Markup Language\"\u306E\u7565\u3067\u3001\u4EBA\u304C\
  \u8AAD\u3081\u308B\u30C7\u30FC\u30BF\u306E\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\u6A19\
  \u6E96\u3067\u3059\u3002\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3084\u8A00\u8A9E\u9593\
  \u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u3088\u304F\u4F7F\u7528\u3055\u308C\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u306E\u5358\u7D14\u3055\
  \u3068\u8AAD\u307F\u3084\u3059\u3055\u306E\u305F\u3081\u306BYAML\u3092\u597D\u3093\
  \u3067\u4F7F\u7528\u3057\u3001\u8A2D\u5B9A\u3001\u3055\u307E\u3056\u307E\u306A\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u69CB\u6210\u3001\u307E\u305F\u306F\
  \u975E\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3088\u308B\u7DE8\u96C6\u304C\u5FC5\
  \u8981\u306A\u30B3\u30F3\u30C6\u30F3\u30C4\u306B\u9069\u3057\u3066\u3044\u307E\u3059\
  \u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法:
LuaにはYAMLの組み込みサポートはありませんが、`lyaml`などのサードパーティーライブラリを使用してYAMLファイルを扱うことができます。このライブラリを使うと、YAMLデータのエンコードとデコードをLuaで行うことができます。まず、LuaのパッケージマネージャーであるLuaRocksを利用して、`lyaml`をインストールする必要があります:

```bash
luarocks install lyaml
```

### YAMLのデコード:
次のYAML内容が`config.yaml`というファイルにあるとします:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

以下のコードを使って、このYAMLファイルをLuaテーブルにデコードできます:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

このスクリプトを実行すると、次のような出力がされます:

```output
host: localhost
port: 3306
username: user
password: pass
```

### YAMLのエンコード:
YAML形式にLuaテーブルをエンコードするには、`lyaml`によって提供される`dump`関数を使用します。次のLuaテーブルのYAML表現を作成したいと考えています:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

出力されるYAMLは次のようになります:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

これらの手順を踏むことで、LuaプログラマはさまざまなアプリケーションのためのYAMLデータを効果的に管理できます。YAMLを使ったこれらの操作は、他のシステムのパーツや他のシステムと直接やり取りするLuaアプリケーションを開発する上で極めて重要です。
