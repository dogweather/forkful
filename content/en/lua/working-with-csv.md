---
title:                "Working with csv"
html_title:           "Lua recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma Separated Values) is a popular file format for storing and transferring tabular data. It consists of rows and columns of data, with each cell containing a value separated by a comma. Programmers often work with CSV files because they are lightweight, easy to read and write, and widely supported by many programming languages and applications.

## How to:
To work with CSV files in Lua, we can use the built-in ```io``` library, which provides functions for handling input/output operations. For example, to read data from a CSV file named "data.csv", we can use the ```io.open()``` function to open the file and the ```io.read()``` function to read its contents:

```
-- Open the CSV file in "r" mode for reading
local file = io.open("data.csv", "r")

-- Read the contents of the file
local data = file:read("*all")

-- Close the file
file:close()

-- Print the data
print(data)
```

This will print all the data from the CSV file to the console. We can also use the ```string.gmatch()``` function to iterate through each line of data and extract individual values from each cell using a comma as the delimiter:

```
-- Open the CSV file in "r" mode for reading
local file = io.open("data.csv", "r")

-- Iterate through each line of data
for line in file:lines() do
  -- Extract the values from each cell using a comma as the delimiter
  local value1, value2, value3 = string.gmatch(line, "([^,]+),([^,]+),([^,]+)")

  -- Print the values
  print(value1, value2, value3)
end

-- Close the file
file:close()
```

This will print each value from the CSV file to the console, separated by a tab. It is important to note that the ```io.open()``` function returns a file object, which we can then use to perform various operations on the file, such as reading or writing data.

## Deep Dive:
CSV files have been around since the early days of computing and were originally used to transfer data between mainframe computers. They gained widespread popularity with the rise of personal computers in the 1980s and have since become a standard format for storing and sharing data.

While CSV files are simple and widely supported, they have some limitations, such as not being able to represent complex data structures or handle special characters like commas within a cell. As an alternative, programmers can use libraries such as LuaCSV or LuaDataTables, which provide more robust features for working with CSV files.

When working with CSV files in Lua, it is important to be mindful of the different newline characters used on different operating systems. For example, Windows uses a carriage return (CR) and a line feed (LF) for a new line, while Unix systems use only a LF. This can cause issues when trying to read or write data from CSV files across different platforms.

## See Also:
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) for more information on the ```io``` library in Lua.
- [LuaCSV documentation](https://www.lua.org/extras/5.1/csv/) for a popular Lua library for parsing CSV files.
- [LuaDataTables documentation](https://github.com/moteus/lua-datatables) for another Lua library for working with CSV files.