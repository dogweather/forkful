---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
JSON是一种轻量级的数据交换格式，程序员用它来存储和传输数据。它易于人阅读和编写，同时也便于机器解析和生成。

## 如何操作：
```Fish Shell
# 安装jq工具
sudo apt install jq

# 解析JSON文件
cat data.json | jq '.'

# 获取特定字段的值
echo '{"name": "Fish", "type": "Shell"}' | jq '.name'
# 输出: "Fish"

# 修改字段值
echo '{"name": "Fish", "type": "Shell"}' | jq '.name="Bash"'
# 输出: {
#   "name": "Bash",
#   "type": "Shell"
# }
```

## 深入了解
JSON诞生于2000年，由Douglas Crockford提出。它现已普遍用于API数据交换和配置文件。除了`jq`，还有其他工具如`jshon`和编程语言内置库（例如Python的`json`模块），它们各有特色。`jq`以其强大的查询和转换功能著称，极适合在shell脚本中使用。

## 参见
- `jq`官方文档: https://stedolan.github.io/jq/manual/
- JSON官方网站: https://www.json.org/json-zh.html
- Fish Shell官网：https://fishshell.com/docs/current/index.html
