---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:59.588697-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua\u6CA1\u6709\u5185\u7F6E\u7684HTML\u89E3\
  \u6790\u5E93\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528\u7B2C\u4E09\u65B9\u5E93\u5982\
  `LuaHTML`\uFF0C\u6216\u901A\u8FC7`LuaXML`\u5229\u7528`libxml2`\u7684\u7ED1\u5B9A\
  \u3002\u4E00\u4E2A\u6D41\u884C\u7684\u65B9\u6CD5\u662F\u4F7F\u7528`lua-gumbo`\u5E93\
  \u6765\u89E3\u6790HTML\uFF0C\u5B83\u63D0\u4F9B\u4E86\u4E00\u4E2A\u76F4\u63A5\u3001\
  \u7B26\u5408HTML5\u6807\u51C6\u7684\u89E3\u6790\u80FD\u529B\u3002"
lastmod: '2024-04-05T21:53:48.214178-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
Lua没有内置的HTML解析库，但你可以利用第三方库如`LuaHTML`，或通过`LuaXML`利用`libxml2`的绑定。一个流行的方法是使用`lua-gumbo`库来解析HTML，它提供了一个直接、符合HTML5标准的解析能力。

### 安装lua-gumbo:
首先，确保安装了`lua-gumbo`。你通常可以使用luarocks来安装它：

```sh
luarocks install lua-gumbo
```

### 使用lua-gumbo进行基本解析：
这里展示了如何使用`lua-gumbo`解析一个简单的HTML片段并从中提取数据：

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>你好，世界！</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- 输出：你好，世界！
```

### 进阶示例 - 提取链接：
要从HTML文档中提取所有锚标签（`<a>`元素）的`href`属性：

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>示例页面</title></head>
<body>
  <a href="http://example.com/1">链接1</a>
  <a href="http://example.com/2">链接2</a>
  <a href="http://example.com/3">链接3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- 确保它是一个元素并且具有属性
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- 样例输出：
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

这段代码片段遍历文档中的所有链接并打印它们的`href`属性。`lua-gumbo`库解析并理解HTML文档的结构的能力简化了基于标签或属性提取特定元素的过程。
