---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:42.235067-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish Shell \u672C\u8EAB\u6CA1\u6709\u7528\
  \u4E8E\u89E3\u6790\u548C\u751F\u6210 JSON \u7684\u5185\u7F6E\u5DE5\u5177\u3002\u7136\
  \u800C\uFF0C\u5B83\u53EF\u4EE5\u4E0E\u7B2C\u4E09\u65B9\u5DE5\u5177\u5982 `jq` \u65E0\
  \u7F1D\u96C6\u6210\uFF0C\u7528\u4E8E JSON \u5904\u7406\u3002`jq` \u662F\u4E00\u4E2A\
  \u529F\u80FD\u5F3A\u5927\u4E14\u591A\u624D\u591A\u827A\u7684\u547D\u4EE4\u884C JSON\
  \ \u5904\u7406\u5668\uFF0C\u5141\u8BB8\u4F60\u4F7F\u7528\u7B80\u5355\u4E14\u5BCC\
  \u6709\u8868\u8FBE\u529B\u7684\u8BED\u8A00\u6765\u5207\u7247\u3001\u8FC7\u6EE4\u3001\
  \u6620\u5C04\u548C\u8F6C\u6362\u7ED3\u6784\u5316\u6570\u636E\u3002"
lastmod: '2024-04-05T21:53:48.565828-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
