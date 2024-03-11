---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:42.235067-07:00
description: "\u5728 Fish Shell \u4E2D\u5904\u7406 JSON \u6D89\u53CA\u89E3\u6790\u548C\
  \u751F\u6210 JSON \u6570\u636E\uFF0C\u8FD9\u5BF9\u4E8E\u914D\u7F6E\u5E94\u7528\u7A0B\
  \u5E8F\u3001API \u4EA4\u4E92\u548C\u7B80\u5316\u547D\u4EE4\u884C\u5DE5\u4F5C\u6D41\
  \u7A0B\u662F\u4E00\u4E2A\u5E38\u89C1\u7684\u4EFB\u52A1\u3002\u9274\u4E8E JSON \u5728\
  \ Web \u548C\u5E94\u7528\u5F00\u53D1\u4E2D\u7684\u666E\u904D\u6027\uFF0C\u5728 shell\
  \ \u4E2D\u76F4\u63A5\u638C\u63E1\u5176\u64CD\u4F5C\u53EF\u4EE5\u663E\u8457\u63D0\
  \u9AD8\u7A0B\u5E8F\u5458\u7684\u81EA\u52A8\u5316\u548C\u6570\u636E\u5904\u7406\u6548\
  \u7387\u3002"
lastmod: '2024-03-11T00:14:22.098747-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Fish Shell \u4E2D\u5904\u7406 JSON \u6D89\u53CA\u89E3\u6790\u548C\
  \u751F\u6210 JSON \u6570\u636E\uFF0C\u8FD9\u5BF9\u4E8E\u914D\u7F6E\u5E94\u7528\u7A0B\
  \u5E8F\u3001API \u4EA4\u4E92\u548C\u7B80\u5316\u547D\u4EE4\u884C\u5DE5\u4F5C\u6D41\
  \u7A0B\u662F\u4E00\u4E2A\u5E38\u89C1\u7684\u4EFB\u52A1\u3002\u9274\u4E8E JSON \u5728\
  \ Web \u548C\u5E94\u7528\u5F00\u53D1\u4E2D\u7684\u666E\u904D\u6027\uFF0C\u5728 shell\
  \ \u4E2D\u76F4\u63A5\u638C\u63E1\u5176\u64CD\u4F5C\u53EF\u4EE5\u663E\u8457\u63D0\
  \u9AD8\u7A0B\u5E8F\u5458\u7684\u81EA\u52A8\u5316\u548C\u6570\u636E\u5904\u7406\u6548\
  \u7387\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
