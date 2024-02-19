---
aliases:
- /zh/bash/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:35.159770-07:00
description: "\u5728Bash\u7F16\u7A0B\u4E2D\u5904\u7406JSON\u6D89\u53CA\u5230\u4ECE\
  \u547D\u4EE4\u884C\u76F4\u63A5\u89E3\u6790\u3001\u63D0\u53D6\u548C\u64CD\u4F5CJSON\u6570\
  \u636E\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u65E0\
  \u7F1D\u5730\u5C06shell\u811A\u672C\u4E0E\u7F51\u7EDCAPI\u548C\u73B0\u4EE3\u6570\
  \u636E\u4EA4\u6362\u683C\u5F0F\u6574\u5408\u5728\u4E00\u8D77\uFF0C\u4F7F\u5F97Bash\u811A\
  \u672C\u5728\u4E00\u4E2A\u91CD\u5EA6\u4F9D\u8D56JSON\u7684\u751F\u6001\u7CFB\u7EDF\
  \u4E2D\u66F4\u52A0\u5F3A\u5927\u548C\u76F8\u5173\u3002"
lastmod: 2024-02-18 23:08:59.311316
model: gpt-4-0125-preview
summary: "\u5728Bash\u7F16\u7A0B\u4E2D\u5904\u7406JSON\u6D89\u53CA\u5230\u4ECE\u547D\
  \u4EE4\u884C\u76F4\u63A5\u89E3\u6790\u3001\u63D0\u53D6\u548C\u64CD\u4F5CJSON\u6570\
  \u636E\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u65E0\
  \u7F1D\u5730\u5C06shell\u811A\u672C\u4E0E\u7F51\u7EDCAPI\u548C\u73B0\u4EE3\u6570\
  \u636E\u4EA4\u6362\u683C\u5F0F\u6574\u5408\u5728\u4E00\u8D77\uFF0C\u4F7F\u5F97Bash\u811A\
  \u672C\u5728\u4E00\u4E2A\u91CD\u5EA6\u4F9D\u8D56JSON\u7684\u751F\u6001\u7CFB\u7EDF\
  \u4E2D\u66F4\u52A0\u5F3A\u5927\u548C\u76F8\u5173\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
