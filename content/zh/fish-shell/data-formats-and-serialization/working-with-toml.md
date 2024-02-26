---
date: 2024-01-26 04:21:37.496676-07:00
description: "TOML \u662F\u4E00\u79CD\u914D\u7F6E\u6587\u4EF6\u683C\u5F0F\uFF0C\u5BF9\
  \u4EBA\u7C7B\u6765\u8BF4\u6613\u4E8E\u8BFB\u5199\uFF0C\u5BF9\u673A\u5668\u6765\u8BF4\
  \u6613\u4E8E\u89E3\u6790\u548C\u751F\u6210\u3002\u5F53\u9879\u76EE\u4E2D\u53EF\u8BFB\
  \u6027\u81F3\u5173\u91CD\u8981\u65F6\uFF0C\u7A0B\u5E8F\u5458\u4F1A\u4F7F\u7528 TOML\
  \ \u6765\u5904\u7406\u6E05\u6670\u3001\u5C42\u6B21\u5206\u660E\u7684\u914D\u7F6E\
  \u6587\u4EF6\u3002"
lastmod: '2024-02-25T18:49:45.850858-07:00'
model: gpt-4-0125-preview
summary: "TOML \u662F\u4E00\u79CD\u914D\u7F6E\u6587\u4EF6\u683C\u5F0F\uFF0C\u5BF9\u4EBA\
  \u7C7B\u6765\u8BF4\u6613\u4E8E\u8BFB\u5199\uFF0C\u5BF9\u673A\u5668\u6765\u8BF4\u6613\
  \u4E8E\u89E3\u6790\u548C\u751F\u6210\u3002\u5F53\u9879\u76EE\u4E2D\u53EF\u8BFB\u6027\
  \u81F3\u5173\u91CD\u8981\u65F6\uFF0C\u7A0B\u5E8F\u5458\u4F1A\u4F7F\u7528 TOML \u6765\
  \u5904\u7406\u6E05\u6670\u3001\u5C42\u6B21\u5206\u660E\u7684\u914D\u7F6E\u6587\u4EF6\
  \u3002"
title: "\u4F7F\u7528TOML"
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
