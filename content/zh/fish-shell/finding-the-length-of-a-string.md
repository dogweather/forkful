---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在编程中，我们可能会寻找字符串的长度。这是因为，了解字符串的长度可以帮助我们更有效地处理数据，更好地进行算法操作，如截取、比较等。

## 如何做:

在 Fish Shell 中，我们可以使用内建的 `string length` 命令来找到字符串的长度。看一下下面的例子:

```Fish Shell
set my_string "hello, world"
string length $my_string
```

这段代码的输出是 `12`，这是因为 "hello, world" 这个字符串中共有12个字符。

## 深入挖掘:

Fish Shell 这个命令行工具在2005年首次发布，很早就提供了内置的 `string length` 命令，以便用户方便地操作字符串。这种简洁高效的设计是 Fish Shell 的一大特色。

同时，Fish Shell 也提供了许多其他的字符串操作命令，比如 `string match`、`string replace` 等等。

在执行 `string length` 时，Fish Shell 实际上会扫描整个字符串，计算其中的字符数目。所以，虽然 `string length` 对于用户来说很简便，但它的复杂度是O(n) -- n是字符串的字符数目。

## 参见:

- Fish Shell 文档: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- 关于字符串操作的更多深入讨论: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)