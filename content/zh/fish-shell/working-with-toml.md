---
title:                "使用TOML"
date:                  2024-01-26T04:21:37.496676-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么以及为什么?
TOML 是一种配置文件格式，对人类来说易于读写，对机器来说易于解析和生成。当项目中可读性至关重要时，程序员会使用 TOML 来处理清晰、层次分明的配置文件。

## 如何操作:
要在 Fish 中读取和操作 TOML，你可能会使用像 `yj` 这样的工具，它可以将 TOML 转换为 JSON。下面是如何操作的说明：

```fish
# 通过 Fisher 安装 yj
fisher install jorgebucaran/yj

# 将 TOML 转换为 JSON
echo 'title = "TOML Example"' | yj -tj

# 示例输出
{"title":"TOML Example"}
```

要写 TOML，你需要反向操作：

```fish
# 将 JSON 转换为 TOML
echo '{"title":"JSON Example"}' | yj -jt

# 示例输出
title = "JSON Example"
```

对于繁重的操作，考虑使用专用的 TOML CLI 工具，比如 `toml-cli`。

```fish
# 安装 toml-cli
pip install toml-cli

# 在 TOML 文件中设置一个值
toml set pyproject.toml tool.poetry.version "1.1.4"

# 从 TOML 文件中获取一个值
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## 深入了解
TOML (Tom's Obvious, Minimal Language)，由 Tom Preston-Werner 在 2013 年提出，类似于 INI 但具有定义明确的规范和数据层次结构。JSON 和 YAML 是主要的替代品，但它们各有所长和所短：JSON 对人不够友好，而 YAML 更复杂。TOML 的设计在配置文件经常需要手工维护的场景中脱颖而出，实现了简单性和表达能力的平衡。在实现方面，大多数编程语言都有 TOML 解析器，包括适用于 Fish 的 TomlBombadil，可以直接嵌入到你的脚本中。

## 另请参阅
- TOML 官方规范：https://toml.io
- `yj`，一个可以在 TOML、JSON、YAML 和 XML 之间转换的工具：https://github.com/jorgebucaran/yj
- `toml-cli`，一个命令行工具，用于 TOML：https://github.com/sdispater/toml-cli
