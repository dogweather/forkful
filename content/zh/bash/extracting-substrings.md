---
title:                "Bash: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

所谓提取子字符串是指从一个较长的字符串中截取出一段较短的字符串。这个过程在Bash编程中经常用到，特别是当我们需要对字符串进行一些操作时。提取子字符串可以帮助我们更方便地处理数据，让我们的代码更加简洁高效。

## 如何

在Bash中，我们可以使用`substr`命令来提取子字符串。它的基本语法如下：

```Bash
${string:position:length}
```

其中，`string`是我们要提取子字符串的原字符串，`position`表示从哪个位置开始提取，`length`表示要提取的长度。下面是一个例子：

```Bash
text="Hello World"
echo ${text:6:5}
```

输出结果为 `World`，因为我们从第6个位置开始提取，长度为5个字符。

除了使用具体的位置和长度，我们也可以使用特殊的符号来提取子字符串。比如，`#`表示从头开始提取到某个字符，`%`表示从尾部开始提取到某个字符。下面是一个例子：

```Bash
text="Hello World"
echo ${text#H*e}
```

输出结果为 `llo World`，因为我们从头开始提取，直到遇到`e`字符为止。

## 深入探讨

除了使用`substr`命令，我们还可以使用正则表达式来提取子字符串。Bash中的`[[ ]]`括号中可以使用正则表达式来匹配字符串，并使用`=`或`=~`来提取匹配的字符串。比如：

```Bash
text="Hello World"
if [[ $text =~ l+o* ]];
then
  echo ${BASH_REMATCH[0]}
fi
```

输出结果为 `llo`，因为正则表达式`l+o*`匹配到了`llo`这个子字符串，并存储在`BASH_REMATCH[0]`变量中。

值得注意的是，在Bash中，位置和长度都是从0开始计数的。另外，如果我们不指定长度，子字符串会一直提取到字符串结尾。所以，在提取子字符串时，我们需要注意边界情况，避免出现意外的错误。

## 参考链接

- [Bash字符串操作指南](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Bash正则表达式教程](https://www.linuxjournal.com/content/bash-regular-expressions)
- [鸟哥的Linux私房菜：Bash字符串与子字符串的提取](http://cn.linux.vbird.org/linux_basic/0340bashshell-scripts_8.php#stringUtils)