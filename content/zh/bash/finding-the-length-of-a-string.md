---
title:                "Bash: 字符串长度的寻找"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么
有时候我们需要知道字符串的长度，这在处理文本数据时非常重要。通过学习如何找到字符串的长度，你可以更有效地操作文本数据，提高编程能力。

## 如何做
首先，我们需要一个字符串变量来存储我们想要计算长度的文本。在Bash中，可以通过使用`=`符号来定义一个变量，并在变量名前加上美元符号`$`来调用它。例如，`my_string="Hello World!"`。

接下来，我们可以使用`expr`命令来计算字符串的长度，并将结果存储在一个新的变量中。在`expr`命令中，可以使用`length`关键字来获取字符串的长度，语法为`expr length $my_string`。将结果存储在一个新的变量（例如`my_string_length`）中，然后使用`echo`命令来显示结果`my_string_length`。

下面是一个完整的示例代码，以及它的输出结果：

```Bash
my_string="Hello World!"
my_string_length=`expr length $my_string`
echo $my_string_length
```

输出结果为`12`，因为"Hello World!"这个字符串一共有12个字符。

## 深入了解
要计算字符串的长度，实际上就是在计算字符串中包含的字符数。在Bash中，字符串是以字符的形式存储的，每个字符都有一个对应的ASCII码值。因此，计算字符串的长度实际上就是计算字符串中包含的字符数，并不是计算字符串的字节数。这一点在处理中文等多字节字符时尤为重要。

另外，我们还可以使用`wc`命令来计算字符串的长度，语法为`echo $my_string | wc -c`。这条命令会计算出包含换行符在内的字符串的总字符数，因此需要用变量`$my_string`的值作为输入，并通过管道（`|`）传递给`wc -c`命令。

## 参考资料
- [Bash变量](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Bash中的字符串操作](https://www.thegeekstuff.com/2010/07/bash-string-manipulation/)
- [Bash中的expr命令](https://www.radford.edu/~mhtay/CPSC120/0110-BASH-expr.html)
- [更多关于wc命令](https://ss64.com/bash/wc.html)

## 参见
- [我的博客](https://www.example.com)
- [Bash官方文档](https://www.gnu.org/software/bash/manual/)
- [Bash教程（英文）](https://www.tutorialspoint.com/unix/shell_scripting.htm)

*本文仅供学习使用。*