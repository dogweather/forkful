---
title:                "Fish Shell: 将字符串转换为小写。"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

用Fish Shell 编程的Mandarin读者的非正式博客文章 


## 为什么要将字符串转换为小写 

有时候，在编程过程中，我们需要将用户输入的字符串转换为小写。这样可以方便我们进行操作和比较。 

## 如何做到 

使用Fish Shell内置的`string tolower`命令，我们可以轻松地将字符串转换为小写。下面是一个简单的例子： 

```Fish Shell
set str "Hello World"
echo $str | string tolower
```
输出: 
```
hello world
```

## 深入探讨 
为了更深入地了解如何将字符串转换为小写，我们可以研究一下这个命令的工作原理。当我们使用`string tolower`命令时，Fish Shell会将字符串中的所有大写字母转换为对应的小写字母。这个过程是通过遍历字符串中的每个字符，并将其与ASCII码表中的对应值进行比较来实现的。如果字符的ASCII值大于等于65并且小于等于90，那么它就是大写字母。通过将其ASCII值加上32，就可以得到对应的小写字母。 

## 参考资料 
-[Fish Shell官方文档](https://fishshell.com/docs/current/commands.html#string) 
-[ASCII码表](https://www.asciitable.com/) 

## 参见 
-[Fish Shell字符串处理指南](https://fishshell.com/docs/current/index.html#index-builtin-string-conversion) 
-[如何使用Fish Shell进行字符串比较](https://fishshell.com/docs/current/tutorial.html#compare-strings)