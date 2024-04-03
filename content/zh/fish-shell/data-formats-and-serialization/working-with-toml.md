---
date: 2024-01-26 04:21:37.496676-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u8981\u5728 Fish \u4E2D\u8BFB\u53D6\u548C\
  \u64CD\u4F5C TOML\uFF0C\u4F60\u53EF\u80FD\u4F1A\u4F7F\u7528\u50CF `yj` \u8FD9\u6837\
  \u7684\u5DE5\u5177\uFF0C\u5B83\u53EF\u4EE5\u5C06 TOML \u8F6C\u6362\u4E3A JSON\u3002\
  \u4E0B\u9762\u662F\u5982\u4F55\u64CD\u4F5C\u7684\u8BF4\u660E\uFF1A."
lastmod: '2024-03-13T22:44:48.292424-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u5728 Fish \u4E2D\u8BFB\u53D6\u548C\u64CD\u4F5C TOML\uFF0C\u4F60\u53EF\
  \u80FD\u4F1A\u4F7F\u7528\u50CF `yj` \u8FD9\u6837\u7684\u5DE5\u5177\uFF0C\u5B83\u53EF\
  \u4EE5\u5C06 TOML \u8F6C\u6362\u4E3A JSON\u3002\u4E0B\u9762\u662F\u5982\u4F55\u64CD\
  \u4F5C\u7684\u8BF4\u660E\uFF1A."
title: "\u4F7F\u7528TOML"
weight: 39
---

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
