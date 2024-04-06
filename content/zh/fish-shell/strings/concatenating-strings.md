---
date: 2024-01-20 17:34:42.867040-07:00
description: "How to: \u600E\u9EBC\u505A Fish Shell \u7EC4\u5408\u5B57\u7B26\u4E32\
  \u5F88\u76F4\u63A5\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.529304-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u9EBC\u505A Fish Shell \u7EC4\u5408\u5B57\u7B26\u4E32\u5F88\u76F4\
  \u63A5\u3002"
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
