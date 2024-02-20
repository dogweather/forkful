---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:17.121788-07:00
description: "YAML\u306F\"YAML Ain't Markup\u2026"
lastmod: 2024-02-19 22:05:01.466329
model: gpt-4-0125-preview
summary: "YAML\u306F\"YAML Ain't Markup\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく理由は？

YAMLは"YAML Ain't Markup Language"の略で、人が読めるデータのシリアライズ標準です。設定ファイルや言語間のデータ交換によく使用されます。プログラマーはその単純さと読みやすさのためにYAMLを好んで使用し、設定、さまざまなアプリケーションの構成、または非プログラマーによる編集が必要なコンテンツに適しています。

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
