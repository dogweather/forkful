---
title:    "Fish Shell: 将字符串转换为小写"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##为什么

在编程过程中，有时候我们需要将字符串转换成小写字母。这项操作可能是为了标准化、比较字符串，或者是为了美观打印输出。无论是出于哪种原因，Fish Shell提供了简单的方法来实现这一目的。

##如何进行

```Fish Shell
# 使用内置的string函数来将字符串转换成小写
set string "HELLO WORLD"
set lower (string) | string.lower
echo $lower  # 输出 hello world
```

```Fish Shell
# 使用参数扩展来进行字符串转换
set string "\\\\HELLO WORLD"
set lower "$string" # 使用双引号在第二个反斜杠后插入一个反斜杠
echo $lower # 输出 \hello world
```

##深入探讨

Fish Shell中的string函数和参数扩展都是用来操作字符串的方便工具。当你想要将字符串转换成小写时，可以使用string函数后跟string.lower来实现。参数扩展可以更灵活地在字符串中插入反斜杠，从而使得转换后的字符串输出更加符合需求。请注意，转换后的字符串将只能输出小写字母，如果有其它字符则不受影响。

##参考阅读

- Fish Shell官方手册：https://fishshell.com/docs/current/index.html
- Fish Shell Wiki：https://github.com/fish-shell/fish-shell/wiki