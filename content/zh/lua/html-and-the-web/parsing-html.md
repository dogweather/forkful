---
title:                "解析HTML"
date:                  2024-02-03T19:12:59.588697-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
解析HTML涉及从HTML文档中提取数据和信息，这对于网页抓取、数据分析和自动化任务至关重要。程序员执行此操作以通过编程方式收集、分析或操作网络内容，实现自动化地从网站中提取数据，而这些任务否则需要手动完成。

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
