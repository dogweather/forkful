---
date: 2024-02-03 19:03:27.415894-07:00
description: "Working with CSV (Comma-Separated Values) files involves parsing and\
  \ generating text data organized into rows and columns, using commas to separate\u2026"
lastmod: 2024-02-19 22:05:18.687401
model: gpt-4-0125-preview
summary: "Working with CSV (Comma-Separated Values) files involves parsing and generating\
  \ text data organized into rows and columns, using commas to separate\u2026"
title: Working with CSV
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) files involves parsing and generating text data organized into rows and columns, using commas to separate individual values. Programmers often engage in this process to facilitate data exchange between different applications, databases, or for data processing and analysis tasks, due to CSV's widespread support and simplicity.

## How to:

In Lua, working with CSV files can be approached using basic file IO operations provided by the language, without the need for external libraries for simple tasks. For more complex operations, such as handling special cases (e.g., commas within values), it might be beneficial to use third-party libraries like `lua-csv`.

### Reading a CSV file
Here’s a simple example to read a CSV file line by line, splitting each line into values based on the comma separator.

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

**Sample output** (for an `example.csv` with content "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Writing a CSV file
To generate a CSV file, you simply construct strings with comma-separated values and write them to a file line by line.

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

This would create (or overwrite) an `output.csv` file with the specified data.

### Using lua-csv
For more advanced CSV handling, including support for quotes and escape characters, the `lua-csv` library is a robust choice.

First, install it using LuaRocks:
```shell
luarocks install lua-csv
```

Then, reading a CSV file becomes as simple as:

```lua
local csv = require("csv")

-- Reading from a file
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

And writing to a CSV with proper quoting and escaping:

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

This approach automatically handles complexities such as commas and quotes within values.
