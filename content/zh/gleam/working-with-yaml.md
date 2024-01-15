---
title:                "使用Yaml编程"
html_title:           "Gleam: 使用Yaml编程"
simple_title:         "使用Yaml编程"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# 为什么？
假设你有一个复杂的程序，需要从不同的环境中读取不同的配置。这时候，YAML文件就可以帮助你轻松处理这些各不相同的设置。它易于阅读和编写，是许多团队和项目中常用的配置格式。

## 如何使用 YAML
```Gleam
import gleam/yaml

// 从 YAML 文件中读取配置数据
let config = gleam/yaml.from_file("config.yml")

// 通过键名获取设置值
let port = gleam/yaml.get(config, "server.port")
```

使用 `from_file` 函数可以从 YAML 文件中读取配置数据。然后，我们可以通过 `get` 函数来获取特定键的值。如果你想要修改配置文件，可以使用 `set` 函数来更新键值。更多用法可以查看官方文档。

```yaml
# config.yml
server:
  port: 8000
database:
  host: "localhost"
  port: 5432
```

假设我们有一个以上的 YAML 文件，每个文件都有自己特定的键值对。我们可以使用 `merge` 函数将它们合并成一个配置文件，这样就可以轻松地管理多个环境的配置了。

```Gleam
// 合并两个 YAML 文件
let common_config = gleam/yaml.from_file("common.yml")
let env_config = gleam/yaml.from_file("production.yml")
let config = gleam/yaml.merge(common_config, env_config)
```

## 深入了解 YAML
YAML 是一种结构化数据格式，它在存储和表示不同类型的数据时非常方便。它支持整数、浮点数、字符串、布尔值、列表和映射等基本数据类型。有时候，我们可能需要对配置文件进行验证，避免类型错误。这时候，你可以使用 `map_and_validate` 函数。此外，还有许多其他的函数可以在 Gleam 的 YAML 模块中找到。

# 参考链接
- [官方网站](https://gleam.run/lib/yaml)
- [YAML 文档](https://yaml.org/)