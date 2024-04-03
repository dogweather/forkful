---
date: 2024-01-26 03:39:34.587273-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish \u5185\u7F6E\u4E86\u8FD9\u7C7B\u4EFB\
  \u52A1\u7684\u9B54\u6CD5\u3002\u4F7F\u7528 `string` \u51FD\u6570\uFF0C\u8F7B\u677E\
  \u5B8C\u6210\u3002\u6765\u770B\u8FD9\u4E9B\u5492\u8BED\uFF1A."
lastmod: '2024-03-13T22:44:48.249743-06:00'
model: gpt-4-0125-preview
summary: "Fish \u5185\u7F6E\u4E86\u8FD9\u7C7B\u4EFB\u52A1\u7684\u9B54\u6CD5\u3002\u4F7F\
  \u7528 `string` \u51FD\u6570\uFF0C\u8F7B\u677E\u5B8C\u6210\u3002\u6765\u770B\u8FD9\
  \u4E9B\u5492\u8BED\uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 如何操作：
Fish 内置了这类任务的魔法。使用 `string` 函数，轻松完成。来看这些咒语：

```fish
# 单引号示例
set quoted "'你好，世界！'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # 输出：你好，世界！

# 双引号情况也一样
set double_quoted "\"你好，宇宙！\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # 输出：你好，宇宙！
```

## 深入探讨
回到命令行的石器时代，你得用 `sed` 或 `awk` 来剥离引号；一大串反斜杠和神秘的标志，真是让人头疼。Fish 的 `string` 函数来自一个更新的时代，让代码更干净、更直观。

其他 shell 的替代方法可能仍依赖于这些旧工具，或者可能使用它们自己的内置方法，如 bash 的参数扩展或 zsh 的修饰符。

`string` 函数不仅限于修剪引号。它是 Fish 中用于字符串操作的瑞士军刀。使用 `string`，你可以在终端里对字符串进行切片、切块、分割、连接，甚至是正则表达式匹配。

## 参见
深入了解 `string`，可以参考官方文档：
- [Fish Shell 字符串文档](https://fishshell.com/docs/current/commands.html#string)

对于怀旧或使用更传统 shell 脚本编写，可以查看：
- [Sed & Awk 指南](https://www.grymoire.com/Unix/Sed.html)
- [Bash 参数扩展](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
