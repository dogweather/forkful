---
title:                "Starting a new project"
aliases:
- /en/lua/starting-a-new-project/
date:                  2024-01-20T18:04:16.692993-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project means you're setting up the groundwork for your brilliant idea. Programmers kick things off to transform concepts into code that practically does something useful.

## How to:
```Lua
-- Let's get cooking with a new Lua project

-- 1. Hello World - The classic start
print("Hello, World!")

-- Sample output: Hello, World!

-- 2. Defining a function - A step further
function greet(name)
    print("Hello, " .. name .. "!")
end

-- Call the function with a name
greet("Lua Programmer")

-- Sample output: Hello, Lua Programmer!

-- 3. Using tables - Lua's way to handle data structures
local inventory = {
    ["apples"] = 10,
    ["oranges"] = 5,
    ["bananas"] = 3
}

-- Add a function to update inventory
function addFruit(fruit, quantity)
    if inventory[fruit] then
        inventory[fruit] = inventory[fruit] + quantity
    else
        inventory[fruit] = quantity
    end
end

-- Call the function to update inventory
addFruit("apples", 5)

-- Output the updated inventory count for apples
print("Apples in inventory: " .. inventory["apples"])

-- Sample output: Apples in inventory: 15
```

## Deep Dive
Lua, birthed in 1993, kept it sweet and simple. It's lightweight, easy to embed, and its table data structures are a flexible way to organize your project's data. Unlike other languages that might offer a dizzying array of data types, Lua sticks to a few core types and uses tables intelligently to make up for it. As for alternatives, you have plenty—Python, Ruby, Node.js, and more, each with their setup quirks and libraries. But if you want a neat, nimble language for a quick spin-up or embedding, Lua's your must-go. Implementation-wise, Lua is all about functions, tables, and simplicity. The lack of redundancy (think classes or complex inheritances) isn't a lack of power; it's a design choice to keep you skating smoothly on your coding journey.

## See Also
- [Official Lua Documentation](https://www.lua.org/manual/5.4/)
- [Programming in Lua (First edition)](https://www.lua.org/pil/contents.html)
- [Learn Lua in Y minutes](https://learnxinyminutes.com/docs/lua/)
