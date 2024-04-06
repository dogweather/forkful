---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:35.159770-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Bash\u672C\u8EAB\u7F3A\u4E4F\u5185\u7F6E\
  \u7684JSON\u89E3\u6790\u80FD\u529B\uFF0C\u4F46\u662F`jq`\u662F\u4E00\u4E2A\u5F3A\
  \u5927\u7684\u547D\u4EE4\u884CJSON\u5904\u7406\u5668\uFF0C\u53EF\u4EE5\u586B\u8865\
  \u8FD9\u4E00\u7A7A\u767D\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\
  \u65B9\u6CD5\uFF1A **\u8BFB\u53D6JSON\u6587\u4EF6\uFF1A** \u793A\u4F8B `data.json`."
lastmod: '2024-04-05T21:53:48.286808-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
