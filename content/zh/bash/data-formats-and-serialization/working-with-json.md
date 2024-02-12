---
title:                "使用JSON进行编程"
aliases:
- /zh/bash/working-with-json.md
date:                  2024-02-03T19:21:35.159770-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在Bash编程中处理JSON涉及到从命令行直接解析、提取和操作JSON数据。程序员经常这样做是为了无缝地将shell脚本与网络API和现代数据交换格式整合在一起，使得Bash脚本在一个重度依赖JSON的生态系统中更加强大和相关。

## 如何操作：
Bash本身缺乏内置的JSON解析能力，但是`jq`是一个强大的命令行JSON处理器，可以填补这一空白。以下是如何使用它的方法：

**读取JSON文件：**

示例 `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

要从JSON文件中读取并提取名称：
```bash
jq '.name' data.json
```
输出：
```
"Jane Doe"
```

**修改JSON数据：**

要更新城市为"Los Angeles"并写回到文件中：
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**从变量解析JSON：**

如果你有一个Bash变量中的JSON，`jq`仍然可以处理它：
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
输出：
```
"John Doe"
```

**处理数组：**

给出一个JSON中的项目数组：
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

要提取第二个项目（索引从0开始）：
```bash
jq '.items[1]' data.json
```
输出：
```
"banana"
```

对于更复杂的操作和过滤，`jq`有一个全面的手册和在线教程，使其成为满足你所有Bash/JSON需求的多功能工具。
