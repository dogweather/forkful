---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:28.823117-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Lua \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528\u8BED\u8A00\u63D0\u4F9B\u7684\u57FA\u672C\u6587\u4EF6 IO \u64CD\u4F5C\u5904\
  \u7406 CSV \u6587\u4EF6\uFF0C\u5BF9\u4E8E\u7B80\u5355\u4EFB\u52A1\u800C\u8A00\uFF0C\
  \u65E0\u9700\u5916\u90E8\u5E93\u3002\u800C\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u64CD\
  \u4F5C\uFF0C\u4F8B\u5982\u5904\u7406\u7279\u6B8A\u60C5\u51B5\uFF08\u4F8B\u5982\uFF0C\
  \u503C\u5185\u7684\u9017\u53F7\uFF09\uFF0C\u5219\u53EF\u80FD\u5229\u7528\u7B2C\u4E09\
  \u65B9\u5E93\u50CF`lua-csv`\u8FD9\u6837\u7684\u5E93\u66F4\u4E3A\u6709\u76CA\u3002\
  \ \u8FD9\u91CC\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF0C\u9010\u884C\
  \u8BFB\u53D6 CSV \u6587\u4EF6\uFF0C\u57FA\u4E8E\u9017\u53F7\u5206\u9694\u7B26\u5C06\
  \u6BCF\u884C\u62C6\u5206\u4E3A\u503C\u3002"
lastmod: '2024-03-13T22:44:47.937184-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Lua \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\u7528\u8BED\u8A00\u63D0\u4F9B\u7684\
  \u57FA\u672C\u6587\u4EF6 IO \u64CD\u4F5C\u5904\u7406 CSV \u6587\u4EF6\uFF0C\u5BF9\
  \u4E8E\u7B80\u5355\u4EFB\u52A1\u800C\u8A00\uFF0C\u65E0\u9700\u5916\u90E8\u5E93\u3002\
  \u800C\u5BF9\u4E8E\u66F4\u590D\u6742\u7684\u64CD\u4F5C\uFF0C\u4F8B\u5982\u5904\u7406\
  \u7279\u6B8A\u60C5\u51B5\uFF08\u4F8B\u5982\uFF0C\u503C\u5185\u7684\u9017\u53F7\uFF09\
  \uFF0C\u5219\u53EF\u80FD\u5229\u7528\u7B2C\u4E09\u65B9\u5E93\u50CF`lua-csv`\u8FD9\
  \u6837\u7684\u5E93\u66F4\u4E3A\u6709\u76CA."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 如何操作：
在 Lua 中，可以使用语言提供的基本文件 IO 操作处理 CSV 文件，对于简单任务而言，无需外部库。而对于更复杂的操作，例如处理特殊情况（例如，值内的逗号），则可能利用第三方库像`lua-csv`这样的库更为有益。

### 读取 CSV 文件
这里是一个简单的例子，逐行读取 CSV 文件，基于逗号分隔符将每行拆分为值。

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**示例输出**（针对内容为"name,age\newlineJohn Doe,30\newlineJane Doe,32"的`example.csv`）：
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### 写入 CSV 文件
要生成一个 CSV 文件，你只需构造带有逗号分隔值的字符串，并逐行写入文件即可。

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

这将创建（或覆盖）一个带有指定数据的`output.csv`文件。

### 使用 lua-csv
对于更高级的 CSV 处理，包括对引号和转义字符的支持，`lua-csv`库是一个强大的选择。

首先，使用 LuaRocks 安装它：
```shell
luarocks install lua-csv
```

然后，读取 CSV 文件变得很简单：

```lua
local csv = require("csv")

-- 从文件中读取
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

以及用适当的引用和转义写入 CSV：

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

这种方法自动处理了值内的逗号和引号等复杂情况。
