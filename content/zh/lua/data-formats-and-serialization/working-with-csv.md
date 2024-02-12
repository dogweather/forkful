---
title:                "处理CSV文件"
aliases:
- /zh/lua/working-with-csv/
date:                  2024-02-03T19:20:28.823117-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
