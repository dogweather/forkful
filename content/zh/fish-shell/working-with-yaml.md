---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
YAML是一种数据序列化格式，用于配置文件和数据交换。程序员使用它因为它易于阅读，结构清晰，且跨语言兼容。

## How to (如何操作)
YAML 文件在 Fish Shell 中的处理通常需要外部工具，如 `yq`。以下是使用 `yq` 在 Fish Shell 里读取和修改 YAML 文件的示例。

读取 YAML:

```Fish Shell
echo 'name: Tom\nage: 30' | yq e '.name' -
# 输出: Tom
```

写入 YAML:

```Fish Shell
echo '{name: "Tom", age: 30}' | yq e '.age = 31' -
# 输出更新后的YAML:
# name: Tom
# age: 31
```

## Deep Dive (深入探索)
YAML诞生于2001年，致力于数据可读性和易用性。相比JSON和XML，YAML更适合配置文件。使用外部工具如`yq`是处理 YAML 的常见方法，`yq` 是基于强大的 `jq` 工具。Fish Shell 自身不内置处理 YAML 的功能，因此你需要借助工具或者在其他支持 YAML 的语言中进行处理。

## See Also (另请参阅)
- YAML 官网: https://yaml.org/
- `yq` GitHub 页面: https://github.com/mikefarah/yq 
- Learn X in Y minutes — YAML: https://learnxinyminutes.com/docs/yaml/
