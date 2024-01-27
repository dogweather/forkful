---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means parsing and generating text data delimited by commas. Programmers do it for simplicity and interoperability – almost every system and language supports CSV, making it a no-brainer for data exchange.

## How to:

Let's read and write CSV files with Lua. We'll handle a basic example without external libraries.

**Reading a CSV file:**

```Lua
function read_csv(filepath)
  local results = {}
  local file = assert(io.open(filepath, "r"))
  
  for line in file:lines() do
    table.insert(results, line:split(","))
  end
  
  file:close()
  return results
end

-- A helper function to split strings
function string:split(delimiter)
  local result = {}
  local from = 1
  local delim_from, delim_to = self:find(delimiter, from, true)
  while delim_from do
    table.insert(result, self:sub(from, delim_from - 1))
    from = delim_to + 1
    delim_from, delim_to = self:find(delimiter, from, true)
  end
  table.insert(result, self:sub(from))
  return result
end
```

**Writing to a CSV file:**

```Lua
function write_csv(filepath, data)
  local file = assert(io.open(filepath, "w"))
  
  for _, row in ipairs(data) do
    file:write(table.concat(row, ",") .. "\n")
  end
  
  file:close()
end

-- Sample data
local data = {
  { "Name", "Age", "City" },
  { "Alice", "30", "New York" },
  { "Bob", "25", "Los Angeles" }
}

write_csv("output.csv", data)
```

## Deep Dive

CSV's history dates back to the early days of computing, where simplicity was key. While JSON and XML now offer richer data structures, CSV remains popular due to its readability and ease of editing with spreadsheet software. When it comes to implementation, watch out for fields with commas, newlines or quotes – these should be quoted and/or escaped properly.

## See Also

- The official Lua 5.4 reference manual: https://www.lua.org/manual/5.4/
- RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files: https://tools.ietf.org/html/rfc4180
- Penlight Lua Libraries (for more advanced CSV handling): https://github.com/lunarmodules/Penlight
