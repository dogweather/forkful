---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:28.823117-07:00
description: "\u64CD\u4F5C CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u6D89\u53CA\u89E3\u6790\u548C\u751F\u6210\u7EC4\u7EC7\u6210\u884C\u548C\u5217\u7684\
  \u6587\u672C\u6570\u636E\uFF0C\u4F7F\u7528\u9017\u53F7\u6765\u5206\u9694\u5404\u4E2A\
  \u503C\u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\u8FDB\u884C\u8FD9\u4E00\u8FC7\u7A0B\uFF0C\
  \u4EE5\u4FC3\u8FDB\u4E0D\u540C\u5E94\u7528\u7A0B\u5E8F\u3001\u6570\u636E\u5E93\u4E4B\
  \u95F4\u7684\u6570\u636E\u4EA4\u6362\uFF0C\u6216\u7528\u4E8E\u6570\u636E\u5904\u7406\
  \u548C\u5206\u6790\u4EFB\u52A1\uFF0C\u8FD9\u5F97\u76CA\u4E8ECSV\u7684\u5E7F\u6CDB\
  \u652F\u6301\u548C\u7B80\u5355\u6027\u3002"
lastmod: '2024-03-13T22:44:47.937184-06:00'
model: gpt-4-0125-preview
summary: "\u64CD\u4F5C CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u6D89\
  \u53CA\u89E3\u6790\u548C\u751F\u6210\u7EC4\u7EC7\u6210\u884C\u548C\u5217\u7684\u6587\
  \u672C\u6570\u636E\uFF0C\u4F7F\u7528\u9017\u53F7\u6765\u5206\u9694\u5404\u4E2A\u503C\
  \u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\u8FDB\u884C\u8FD9\u4E00\u8FC7\u7A0B\uFF0C\u4EE5\
  \u4FC3\u8FDB\u4E0D\u540C\u5E94\u7528\u7A0B\u5E8F\u3001\u6570\u636E\u5E93\u4E4B\u95F4\
  \u7684\u6570\u636E\u4EA4\u6362\uFF0C\u6216\u7528\u4E8E\u6570\u636E\u5904\u7406\u548C\
  \u5206\u6790\u4EFB\u52A1\uFF0C\u8FD9\u5F97\u76CA\u4E8ECSV\u7684\u5E7F\u6CDB\u652F\
  \u6301\u548C\u7B80\u5355\u6027\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

操作 CSV（逗号分隔值）文件涉及解析和生成组织成行和列的文本数据，使用逗号来分隔各个值。程序员常常进行这一过程，以促进不同应用程序、数据库之间的数据交换，或用于数据处理和分析任务，这得益于CSV的广泛支持和简单性。

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
