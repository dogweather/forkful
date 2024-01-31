---
title:                "处理 JSON 数据"
date:                  2024-01-19
html_title:           "Bash: 处理 JSON 数据"
simple_title:         "处理 JSON 数据"

category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
处理JSON是对JavaScript对象表示法（JSON）格式数据的读写操作。程序员这样做是为了处理网络请求和配置文件，因为JSON简单、易读且广泛支持。

## How to: (如何操作)
在Bash中处理JSON通常需要第三方工具，如`jq`。以下是如何使用`jq`的简单示例。

安装`jq`：
```Bash
sudo apt-get install jq
```

解析JSON文件：
```Bash
echo '{"name": "张三", "age": 30}' | jq '.'
```
输出：
```JSON
{
  "name": "张三",
  "age": 30
}
```

提取特定字段：
```Bash
echo '{"name": "张三", "age": 30}' | jq '.name'
```
输出：
```JSON
"张三"
```

## Deep Dive (深入了解)
JSON于2001年由道格拉斯·克罗克福德推广。尽管有XML等替代品，JSON以其简约和易用性成为众多API的首选数据格式。在Bash中，尽管内建支持有限，但工具如`jq`和`jshon`提供了强大的解析能力。

## See Also (另见)
- JSON官方网站: [json.org](https://www.json.org/json-zh.html)
- `jq`手册: [stedolan.github.io/jq/manual/](https://stedolan.github.io/jq/manual/)
- Bash scripting教程: [tldp.org/LDP/Bash-Beginners-Guide/html/](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
