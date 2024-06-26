---
date: 2024-01-20 17:56:28.181202-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u547D\u4EE4\u884C\u53C2\u6570\u5E26\
  \u6765\u4E86\u7075\u6D3B\u6027\uFF0C\u8BA9\u53E4\u8001\u7684\u811A\u672C\u5728\u4E0D\
  \u540C\u73AF\u5883\u4E2D\u90FD\u80FD\u7528\u3002Lua\u4ECE5.1\u7248\u672C\u5F00\u59CB\
  \u4F7F\u7528`{...}`\u6765\u6536\u96C6\u53C2\u6570\uFF0C\u66FF\u4EE3\u4E86\u65E7\u7684\
  `arg`\u8868\u3002\u5982\u679C\u9700\u8981\u7A0B\u5E8F\u8DEF\u5F84\uFF0C\u4F7F\u7528\
  `arg[0]`\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.137133-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u547D\u4EE4\u884C\u53C2\u6570\u5E26\u6765\u4E86\
  \u7075\u6D3B\u6027\uFF0C\u8BA9\u53E4\u8001\u7684\u811A\u672C\u5728\u4E0D\u540C\u73AF\
  \u5883\u4E2D\u90FD\u80FD\u7528\u3002Lua\u4ECE5.1\u7248\u672C\u5F00\u59CB\u4F7F\u7528\
  `{...}`\u6765\u6536\u96C6\u53C2\u6570\uFF0C\u66FF\u4EE3\u4E86\u65E7\u7684`arg`\u8868\
  \u3002\u5982\u679C\u9700\u8981\u7A0B\u5E8F\u8DEF\u5F84\uFF0C\u4F7F\u7528`arg[0]`\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
