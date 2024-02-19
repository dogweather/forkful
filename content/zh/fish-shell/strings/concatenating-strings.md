---
aliases:
- /zh/fish-shell/concatenating-strings/
date: 2024-01-20 17:34:42.867040-07:00
description: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u5408\u5E76\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\
  \u4E86\u751F\u6210\u52A8\u6001\u5185\u5BB9\u3001\u6784\u5EFA\u8DEF\u5F84\u6216\u8005\
  \u7EC4\u5408\u6D88\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.510151
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u5C06\u591A\u4E2A\u5B57\u7B26\
  \u4E32\u5408\u5E76\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\
  \u4E86\u751F\u6210\u52A8\u6001\u5185\u5BB9\u3001\u6784\u5EFA\u8DEF\u5F84\u6216\u8005\
  \u7EC4\u5408\u6D88\u606F\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? 什麼 & 為什麼?
字符串拼接就是将多个字符串合并成一个。程序员这么做为了生成动态内容、构建路径或者组合消息。

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
