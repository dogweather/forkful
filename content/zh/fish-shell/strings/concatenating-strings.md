---
date: 2024-01-20 17:34:42.867040-07:00
description: "How to: \u600E\u9EBC\u505A \u62FC\u63A5\u5B57\u7B26\u4E32\u662F\u7F16\
  \u7A0B\u4E2D\u6700\u57FA\u7840\u7684\u529F\u80FD\u4E4B\u4E00\uFF0CFish Shell \u63D0\
  \u4F9B\u7684\u65B9\u6CD5\u7B80\u5355\u4E14\u76F4\u89C2\u3002\u5386\u53F2\u4E0A\uFF0C\
  \u4E0D\u540C\u7684\u7F16\u7A0B\u8BED\u8A00\u6709\u4E0D\u540C\u7684\u62FC\u63A5\u65B9\
  \u6CD5\uFF0C\u8BF8\u5982\u5728 C \u8BED\u8A00\u4E2D\u9700\u8981\u4F7F\u7528\u51FD\
  \u6570 `strcat`\uFF0CPython \u4E2D\u53EF\u4EE5\u7528 `+` \u6216 `.join()`\u3002\
  Fish Shell\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.451070-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u9EBC\u505A \u62FC\u63A5\u5B57\u7B26\u4E32\u662F\u7F16\u7A0B\u4E2D\
  \u6700\u57FA\u7840\u7684\u529F\u80FD\u4E4B\u4E00\uFF0CFish Shell \u63D0\u4F9B\u7684\
  \u65B9\u6CD5\u7B80\u5355\u4E14\u76F4\u89C2\u3002\u5386\u53F2\u4E0A\uFF0C\u4E0D\u540C\
  \u7684\u7F16\u7A0B\u8BED\u8A00\u6709\u4E0D\u540C\u7684\u62FC\u63A5\u65B9\u6CD5\uFF0C\
  \u8BF8\u5982\u5728 C \u8BED\u8A00\u4E2D\u9700\u8981\u4F7F\u7528\u51FD\u6570 `strcat`\uFF0C\
  Python \u4E2D\u53EF\u4EE5\u7528 `+` \u6216 `.join()`\u3002Fish Shell \u7684\u9690\
  \u5F0F\u62FC\u63A5\u7B26\u5408\u5176\u7B80\u6D01\u6613\u7528\u7684\u8BBE\u8BA1\u54F2\
  \u5B66\uFF0C\u800C `string` \u547D\u4EE4\u5219\u63D0\u4F9B\u4E86\u66F4\u591A\u7075\
  \u6D3B\u6027\u548C\u529F\u80FD\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## How to: 怎麼做
Fish Shell 组合字符串很直接。

```Fish Shell
# 使用隐式拼接
set greeting "你好, "
set name "世界!"
echo $greeting$name
# 输出: 你好, 世界!

# 使用 `string` 命令
set full_greeting (string join '' $greeting $name)
echo $full_greeting
# 输出: 你好, 世界!
```

## Deep Dive 深入探討
拼接字符串是编程中最基础的功能之一，Fish Shell 提供的方法简单且直观。历史上，不同的编程语言有不同的拼接方法，诸如在 C 语言中需要使用函数 `strcat`，Python 中可以用 `+` 或 `.join()`。Fish Shell 的隐式拼接符合其简洁易用的设计哲学，而 `string` 命令则提供了更多灵活性和功能。

Fish Shell 的 `string` 命令不仅限于拼接，它也能分割、替换等操作字符串。不过，相比其他语言，Fish 不支持 `+` 操作符用于拼接，而是通过空格来自然拼接或者利用 `string` 命令。

## See Also 相关资源
- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub 仓库](https://github.com/fish-shell/fish-shell)
