---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:17.121788-07:00
description: "\u65B9\u6CD5:\u2026"
lastmod: '2024-03-13T22:44:42.336229-06:00'
model: gpt-4-0125-preview
summary: "Lua\u306B\u306FYAML\u306E\u7D44\u307F\u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\
  \u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001`lyaml`\u306A\u3069\u306E\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u30FC\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\
  \u3057\u3066YAML\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002\u3053\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u3046\
  \u3068\u3001YAML\u30C7\u30FC\u30BF\u306E\u30A8\u30F3\u30B3\u30FC\u30C9\u3068\u30C7\
  \u30B3\u30FC\u30C9\u3092Lua\u3067\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002\u307E\u305A\u3001Lua\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u30DE\u30CD\
  \u30FC\u30B8\u30E3\u30FC\u3067\u3042\u308BLuaRocks\u3092\u5229\u7528\u3057\u3066\
  \u3001`lyaml`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308A\u307E\u3059."
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
