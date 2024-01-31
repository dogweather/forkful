---
title:                "解析HTML"
date:                  2024-01-20T15:32:47.776379-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/parsing-html.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
解析HTML即是指让程序去读懂并操作HTML代码的过程。它是程序员获取网页数据和内容时必不可少的一部分，特别是做网络爬虫或数据挖掘时。

## 如何：
```Lua
-- 需要安装html-parser库
local htmlparser = require "htmlparser"

-- 解析HTML字符串
local html = [[
<!DOCTYPE html>
<html>
<head>
  <title>示例页面</title>
</head>
<body>
  <h1>欢迎访问</h1>
  <p>这是一个简单的HTML页面。</p>
</body>
</html>
]]

local root = htmlparser.parse(html)
local h1s = root:select("h1")

for _, h1 in ipairs(h1s) do
    print(h1:getcontent()) -- 输出所有<h1>标签的内容
end
```
输出：
```
欢迎访问
```

## 深入探讨
最初，HTML被设计成了一个相对简单的结构，方便人们读写和解析。但随着网站功能的复杂化，HTML解析变得越来越重要且复杂。除了Lua的解析器外，还有Python的BeautifulSoup、JavaScript的Cheerio等替代品。在实现细节方面，解析一个HTML通常涉及DOM树的构建、字符编码的处理以及错误处理机制。

## 参见
- [Lua html-parser GitHub](https://github.com/msva/lua-htmlparser)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
