---
title:                "将字符串大写"
html_title:           "Fish Shell: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

字符串大写化是指将字符串中的每个字母改换为大写形式。程序员通常需要这么做，以便在不区分大小写的场景中比较字符串，或者改变字符串的显示方式。

## 怎么做：

在 Fishing Shell 中，我们可以使用 string 的上转换功能来达到这个目的。下面是一个例子：

```Fish Shell
set a 'fish shell'
set a (string upper $a)
echo $a
```

你将会看到下面的结果：

```Fish Shell
FISH SHELL
```

## 深入探索：

字符串大写化在计算机历史中的角色并不凡。在早期的计算机系统中，甚至只有大写字母。这就是为什么在一些古老的系统中，你可能会遇到丧失了大小写敏感性的问题。

有许多方法可以做到字符串的大写化，在 Fish 中除了上述的 `string upper` 命令，你还可以使用 `tr` 命令，这个命令的由来可以追溯到 Unix 的诞生:

```Fish Shell
set a 'fish shell'
echo $a | tr '[:lower:]' '[:upper:]'
```

关于取大写的操作，Fish 实际上是采用了 Unicode 规范中的大写映射来处理的。这就意味着在非英文字母，比如希腊字母或俄语字母中， `string upper` 也能工作得很好。不过值得注意的是，并不是所有的 Unicode 字符都有对等的大写形式。

## 参见：

如果你对 Fish Shell 有进一步的学习需求，可以参阅下面的链接：

[Fish Shell 官方文档](https://fishshell.com/docs/current/index.html) 

[String 上转换函数文档](https://fishshell.com/docs/current/cmds/string-upper.html)