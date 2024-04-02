---
date: 2024-01-20 17:56:28.181202-07:00
description: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u6211\u4EEC\u4ECE\
  \u5916\u90E8\u83B7\u53D6\u4FE1\u606F\uFF0C\u4F20\u9012\u7ED9Lua\u811A\u672C\u3002\
  \u8FD9\u6837\u505A\u53EF\u4EE5\u8BA9\u7A0B\u5E8F\u66F4\u7075\u6D3B\uFF0C\u9002\u5E94\
  \u4E0D\u540C\u7684\u60C5\u51B5\u548C\u7528\u6237\u9700\u6C42\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.928759-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u6211\u4EEC\u4ECE\
  \u5916\u90E8\u83B7\u53D6\u4FE1\u606F\uFF0C\u4F20\u9012\u7ED9Lua\u811A\u672C\u3002\
  \u8FD9\u6837\u505A\u53EF\u4EE5\u8BA9\u7A0B\u5E8F\u66F4\u7075\u6D3B\uFF0C\u9002\u5E94\
  \u4E0D\u540C\u7684\u60C5\u51B5\u548C\u7528\u6237\u9700\u6C42\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## What & Why? 什么和为什么?
读取命令行参数允许我们从外部获取信息，传递给Lua脚本。这样做可以让程序更灵活，适应不同的情况和用户需求。

## How to: 如何操作
```Lua
-- 假设这个Lua脚本被命名为 "example.lua"
-- 在命令行运行: lua example.lua arg1 arg2 arg3

-- 读取参数
local args = {...}  -- 将命令行参数存进 'args' 数组

-- 使用参数
for index, value in ipairs(args) do
    print("参数 " .. index .. ": " .. value)
end
```
输出结果示例:
```
参数 1: arg1
参数 2: arg2
参数 3: arg3
```

## Deep Dive 深入了解
命令行参数带来了灵活性，让古老的脚本在不同环境中都能用。Lua从5.1版本开始使用`{...}`来收集参数，替代了旧的`arg`表。如果需要程序路径，使用`arg[0]`。

替代方案比如环境变量等，通常在不方便直接通过命令行传递参数时使用。实现细节方面，Lua在解析参数时不会处理引号内的空格，这意味着包含空格的参数应该用引号括起来。

## See Also 参考资料
- [Lua 5.4 参考手册](https://www.lua.org/manual/5.4/)
- [Programming in Lua](https://www.lua.org/pil/contents.html)
