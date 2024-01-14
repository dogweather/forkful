---
title:    "Bash: 读取命令行参数"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么会阅读命令行参数

命令行参数是在Bash编程中非常重要的部分。通过阅读命令行参数，我们可以让我们的脚本能够根据用户输入的不同参数来执行不同的操作。这样使得我们的脚本更加灵活和实用。接下来，我们将会介绍如何阅读命令行参数并给出实际的代码示例。

## 如何

阅读命令行参数的语法很简单。我们使用特定的变量来接收用户输入的参数，然后在代码中调用这些变量即可。例如，在下面的代码中，我们使用$1和$2来分别接收用户输入的第一个和第二个参数。

```
Bash代码示例：
#!/bin/bash

echo "您输入的第一个参数是$1"
echo "您输入的第二个参数是$2"
```

以下是我们使用上述脚本的结果：

```
用户输入：
./script.sh Hello World

脚本输出：
您输入的第一个参数是Hello
您输入的第二个参数是World
```

此外，还有许多特殊的变量可以帮助我们读取和使用命令行参数，例如$0表示脚本本身，$@表示所有输入参数的列表等等。通过灵活地使用这些变量，我们可以轻松地处理不同类型的命令行参数。

## 深入

阅读命令行参数还有许多其他的技巧和注意事项。例如，我们可以使用shift命令来移动参数的位置，使得我们可以在脚本中更简洁地处理不同数量的输入参数。此外，还可以使用getopts命令来解析用户输入的选项参数，让我们的脚本更加强大和用户友好。

另外，如果需要用户输入敏感信息，如密码等，我们还可以使用read命令来读取带有-h标志的输入，使得输入信息不会显示在屏幕上，增加了安全性。

阅读命令行参数是Bash编程中不可或缺的一部分，通过深入了解其使用方法和技巧，我们可以写出更加灵活和功能强大的脚本。

## 参考链接

- [Bash Shell Script: Check Command Line Arguments (nixCraft)](https://www.cyberciti.biz/faq/bash-shell-script-check-command-line-arguments/)
- [Manipulating Input Parameters (Linux Documentation Project)](https://tldp.org/LDP/abs/html/internalvariables.html#SUBSREF)
- [Using Shift (Bash Hackers Wiki)](https://wiki.bash-hackers.org/scripting/posparams#shifting)
- [Using Getopts (Bash Hackers Wiki)](https://wiki.bash-hackers.org/howto/getopts_tutorial)
- [Reading User Input with Read (GNU Documentation)](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Builtins) 

## 参考链接