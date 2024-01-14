---
title:    "Bash: 读取命令行参数。"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么
有些人对于Bash编程可能有些感到陌生，但是它实际上是一种强大的工具，可以帮助你快速处理命令行参数。通过阅读本文，你将了解如何看懂命令行参数并使用它来实现你的目标。

## 如何
使用Bash编程来读取命令行参数非常简单。首先，你需要创建一个名为“demo.sh”的文本文件，并且使用我们喜欢的文本编辑器打开它。然后，输入以下内容：
```Bash
#!/bin/bash

echo "第一个参数为$1"
echo "第二个参数为$2"
```
这段代码首先使用`#!/bin/bash`指定Bash作为解释器，然后使用`echo`命令打印出传递给脚本的前两个参数。接下来，我们需要使用命令行来执行这个脚本：
```Bash
$ ./demo.sh Hello World
```
输出应该会是：
```Bash
第一个参数为Hello
第二个参数为World
```

## 深入研究
在Bash编程中，我们可以使用特殊变量来读取命令行参数，如`$1`、`$2`等。这些变量表示传递给脚本的位置参数。如果你想要读取所有的命令行参数，可以使用`$@`特殊变量，它将返回一个包含所有参数的列表。另外，你也可以使用`$#`来获取传递给脚本的参数个数。除此之外，你还可以使用`$0`来获取脚本的名称。

## 参考链接
- Bash入门教程：https://www.runoob.com/linux/linux-shell.html
- Shell脚本入门：https://www.cnblogs.com/qlong8807/p/5771808.html
- Bash手册：https://www.gnu.org/software/bash/manual/bash.html

## 另请参阅 
- Bash参数替换：https://blog.csdn.net/tired_fish/article/details/1889154
- 使用getopt和getopts来处理命令行参数：https://blog.csdn.net/29eec4bcuwonkul/article/details/7303415