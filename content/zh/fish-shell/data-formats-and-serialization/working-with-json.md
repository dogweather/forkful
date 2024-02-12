---
title:                "使用JSON进行编程"
aliases: - /zh/fish-shell/working-with-json.md
date:                  2024-02-03T19:22:42.235067-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

在 Fish Shell 中处理 JSON 涉及解析和生成 JSON 数据，这对于配置应用程序、API 交互和简化命令行工作流程是一个常见的任务。鉴于 JSON 在 Web 和应用开发中的普遍性，在 shell 中直接掌握其操作可以显著提高程序员的自动化和数据处理效率。

## 如何操作：

Fish Shell 本身没有用于解析和生成 JSON 的内置工具。然而，它可以与第三方工具如 `jq` 无缝集成，用于 JSON 处理。`jq` 是一个功能强大且多才多艺的命令行 JSON 处理器，允许你使用简单且富有表达力的语言来切片、过滤、映射和转换结构化数据。

### 使用 jq 解析 JSON
要使用 `jq` 解析一个 JSON 文件并提取数据：

```fish
# 假设你有一个名为 'data.json' 的 JSON 文件，内容为：{"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# 示例输出
"Fish Shell"
```

### 使用 jq 生成 JSON
从 shell 变量或输出创建 JSON 内容：

```fish
# 从变量创建 JSON 对象
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# 示例输出
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### 过滤 JSON 集合
假设我们有一个名为 `versions.json` 的文件，其中包含一个对象数组：
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
要过滤这个数组以仅获取稳定版本：

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# 示例输出
"3.1.2"
"3.4.0"
```

所提供的示例展示了将 `jq` 与 Fish Shell 集成用于 JSON 操作的强大功能。利用这类工具丰富了 shell 体验，使之成为处理现代数据格式的强大环境。
