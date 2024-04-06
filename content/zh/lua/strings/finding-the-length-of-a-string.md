---
date: 2024-01-20 17:47:41.646089-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Lua\u91CC\uFF0C\u5B57\u7B26\u4E32\
  \u662F\u4E0D\u53EF\u53D8\u7684\u503C\u3002\u5F97\u77E5\u5B57\u7B26\u4E32\u957F\u5EA6\
  \u662F\u5E38\u89C1\u7684\u9700\u6C42\uFF0C\u901A\u5E38\u7528\u4E8E\u5FAA\u73AF\u3001\
  \u5206\u5272\u6216\u8005\u5176\u4ED6\u6587\u672C\u5904\u7406\u4EFB\u52A1\u3002\u539F\
  \u751FLua\u4F7F\u7528`#`\u64CD\u4F5C\u7B26\u6216`string.len()`\u51FD\u6570\u6765\
  \u83B7\u53D6\u5B57\u7B26\u4E32\u957F\u5EA6\u3002\u8FD9\u79CD\u8BBE\u8BA1\u601D\u8DEF\
  \u6E90\u4E8ELua\u7684\u7B80\u5355\u548C\u6548\u7387\u539F\u5219\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.059232-06:00'
model: gpt-4-1106-preview
summary: "\u5386\u53F2\u80CC\u666F\u65B9\u9762\uFF0CLua\u81EA5.0\u7248\u672C\u5F15\
  \u5165\u4E86`#`\u64CD\u4F5C\u7B26\uFF0C\u4E4B\u524D\u53EA\u80FD\u901A\u8FC7`string.len()`\u51FD\
  \u6570\u5F97\u77E5\u957F\u5EA6\u3002`#`\u63D0\u4F9B\u4E86\u4FBF\u6377\u7684\u65B9\
  \u5F0F\uFF0C\u4F46\u662F\u4E0D\u5904\u7406UTF-8\u5B57\u7B26\u4E32\u3002\u5728\u591A\
  \u8BED\u8A00\u7F16\u7A0B\u4E2D\uFF0C\u4F7F\u7528`utf8.len()`\u6765\u6B63\u786E\u5904\
  \u7406\u591A\u5B57\u8282\u5B57\u7B26\uFF0C\u8FD9\u5728Lua 5.3\u4E2D\u5F15\u5165\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## 如何操作：
```Lua
-- 使用#操作符
local str = "你好，世界！"
print(#str)  -- 输出字符串长度
```
输出：
```
15
```

```Lua
-- 使用string.len函数
local str = "Lua编程"
print(string.len(str))  -- 同样输出字符串长度
```
输出：
```
9
```

## 深入了解
在Lua里，字符串是不可变的值。得知字符串长度是常见的需求，通常用于循环、分割或者其他文本处理任务。原生Lua使用`#`操作符或`string.len()`函数来获取字符串长度。这种设计思路源于Lua的简单和效率原则。

历史背景方面，Lua自5.0版本引入了`#`操作符，之前只能通过`string.len()`函数得知长度。`#`提供了便捷的方式，但是不处理UTF-8字符串。在多语言编程中，使用`utf8.len()`来正确处理多字节字符，这在Lua 5.3中引入。

替代方案可能涉及自己编写函数来计算字符串长度，特别是处理特殊编码时。Lua中字符串的实现采用内部结构保存长度信息，这就是为什么使用`#`操作符和`string.len()`即快速又准确。

## 查看更多
- Lua 5.4参考手册: http://www.lua.org/manual/5.4/
- Lua字符串教程: https://www.lua.org/pil/21.1.html
- UTF-8支持: https://www.lua.org/manual/5.4/manual.html#6.5
