---
title:    "Bash: 将字符串转换为小写"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么：将字符串转换为小写
在命令行中，你可能经常需要处理不同的文本数据。但是，有时候你可能会遇到一些文本数据全部是大写字母或者混合大小写的情况。为了更好地处理这些数据，你可以使用Bash编程来将字符串转换为小写。

## 如何操作
要将字符串转换为小写，在Bash中可以使用`tr`命令。它可以将输入的文本数据中的字符替换为你指定的字符。在本例中，我们将使用`tr`命令的`[:upper:]`和`[:lower:]`选项来将大写字符替换为小写字符。下面是一个示例代码和输出：

```Bash
# 原始字符串
str="HELLO WORLD"

# 使用tr命令将字符串转换为小写
echo $str | tr '[:upper:]' '[:lower:]'

# 输出结果：hello world
```

可以看到，经过`tr`命令处理后，字符串中所有的大写字母都被转换为了小写字母。

## 深入了解
除了使用`tr`命令，Bash编程还提供了其他方式来将字符串转换为小写。例如，可以使用`declare`命令和`Parameter Expansion`来实现这一功能。同时，在Bash中也可以定义自己的函数来完成字符串转换，具体操作可以参考相关文档。

值得注意的是，在处理字符串时，我们也可以只将特定位置的字符转换为小写，而不是全部的字符串。这就需要我们使用到循环和条件语句来实现。通过这种方式，可以更灵活地处理不同类型的文本数据。

## 参考链接
- Bash文本处理官方文档：https://www.gnu.org/software/bash/manual/html_node/Text-Manipulation.html#Text-Manipulation
- 使用tr命令转换字符串大小写：https://unix.stackexchange.com/questions/226161/converting-string-to-lower-case-in-bash-script
- 使用declare命令和Parameter Expansion转换字符串大小写：https://stackoverflow.com/questions/22698224/bash-script-to-join-two-existing-csv-files-for-merging-using-unique-more-sophis
- Bash中定义函数的方法：https://linuxhandbook.com/bash-functions/ 

# 另请参阅
- Bash编程基础教程：https://linuxize.com/post/bash-scripting-tutorial/
- Bash常用命令速查表：https://www.tecmint.com/linux-commands-cheat-sheet/
- 使用Bash处理文本数据的常用技巧：https://www.tecmint.com/useful-linux-bash-commands/