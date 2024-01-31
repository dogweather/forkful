---
title:                "从字符串中移除引号"
date:                  2024-01-26T03:39:34.587273-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

从字符串中删除引号，是指将文本数据中那些讨厌的单引号 (' ') 或双引号 (" ") 去除。程序员经常这样做，以清理输入或为进一步处理数据而去除引号的杂乱。

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
