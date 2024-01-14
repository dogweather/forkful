---
title:    "Fish Shell: 删除匹配模式的字符"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 为什么 

### 在Fish Shell程序中删除匹配模式的字符

在日常电脑使用中，我们可能会遇到需要删除文本中特定字符的情况，例如删除所有的空格或者特定符号。Fish Shell一种使用方便、功能强大的命令行工具，可以帮助我们轻松地进行此类操作。

# 如何

```Fish Shell
set text "今天天气☀️非常好🌻"
echo $text
```
输出：
今天天气☀️非常好🌻

```Fish Shell
echo $text | tr -d ☀️🌻
```
输出：
今天天气非常好

在上面的例子中，我们使用tr命令（translate）来删除文本中的特定字符。首先，我们设置了一个变量text为“今天天气☀️非常好🌻”，然后使用echo命令打印出来。接着，我们使用管道符号（|）将该文本传递给tr命令。tr命令后的-d参数表示删除匹配到的字符，紧跟着的☀️🌻就是我们要删除的字符。最后，tr命令的输出被返回给echo命令，最终输出为“今天天气非常好”。

# 深入探讨

除了使用tr命令外，我们也可以使用sed命令来实现相同的功能。

```Fish Shell
echo $text | sed 's/☀️//g;s/🌻//g'
```
输出：
今天天气非常好

在上面的例子中，我们使用sed命令（stream editor）来替换文本中的字符。首先，我们同样通过管道符号将文本传递给sed命令。接着，s（substitute）表示替换操作，后面跟着第一个/☀️/表示要替换的字符，第二个//表示替换为空格，最后的g（global）表示替换所有匹配到的字符。在第二个替换中，我们同样将🌻替换为空格。最后，sed命令的输出被返回给echo命令，最终输出为“今天天气非常好”。

# 参考资料

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [Linux命令详解：tr](https://www.jianshu.com/p/a05f3434195e)
- [Linux命令详解：sed](https://www.jianshu.com/p/91b9d86c59d6)

# 参见

- [Fish Shell快速入门指南](https://blog.csdn.net/whb193565/article/details/78486466)
- [Fish Shell实用技巧合集](https://www.jianshu.com/p/c0b8229b9b21)