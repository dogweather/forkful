---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? 什麼和為什麼？
YAML是一種方便的數據序列化格式，常用於配置文件。程序員使用YAML是因為它易於閱讀，簡單，並且能夠很好地與多種編程語言配合。

## How to: 如何進行
Elixir處理YAML需要用到第三方庫，例如`yaml_elixir`。首先，在`mix.exs`文件添加依賴：

```elixir
def deps do
  [
    {:yaml_elixir, "~> 2.0"}
  ]
end
```

然後在終端執行`mix deps.get`。

讀取YAML文件：

```elixir
yaml_content = """
name: Zhang San
age: 30
"""

{:ok, data} = YamlElixir.read_from_string(yaml_content)
IO.inspect(data)
```

預期輸出將是Elixir的Map格式：

```elixir
%{"name" => "Zhang San", "age" => 30}
```

## Deep Dive 深入探索
YAML，代表“YAML Ain't Markup Language”，原先為“Yet Another Markup Language”，強調輕量級和易於人類閱讀。儘管JSON是另一個受歡迎的選擇，但YAML在需要多行字符串和注釋的情景下更受青睞。在Elixir中，解析YAML需要轉接C語言庫如`libyaml`，通過NIF（Native Implemented Functions）或Port與虛擬機溝通。

## See Also 相關資源
- YAML官方網站: https://yaml.org
- yaml_elixir GitHub頁面: https://github.com/KamilLelonek/yaml_elixir
- Hex.pm的YamlElixir: https://hex.pm/packages/yaml_elixir
- Elixir官方文檔: https://elixir-lang.org/docs.html
