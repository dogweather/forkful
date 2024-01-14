---
title:                "Bash: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么阅读命令行参数

阅读命令行参数是学习Bash编程的重要一步。它能够帮助你更好地掌握命令行界面，并且为你的编程工作提供方便。通过阅读命令行参数，你可以轻松地向你的脚本传递信息，控制程序的行为，并且提高生产效率。

## 如何阅读命令行参数

首先，在Bash中使用`$#`来访问命令行参数的数量。例如，如果你运行`bash script.sh apple orange banana`，那么`$#`会返回3个参数的数量。

接下来，你可以使用`$@`来访问所有的命令行参数。它会将所有的参数作为一个字符串返回。例如，在上面的例子中，`$@`会返回`apple orange banana`。

如果你想要访问单独的命令行参数，你可以使用`$1`，`$2`，`$3`等等。它会按照参数的顺序返回对应的参数。例如，在上面的例子中，`$1`会返回`apple`，`$2`会返回`orange`，`$3`会返回`banana`。

这些都是最基本的命令行参数访问方式。你也可以使用`getopts`指令来更有效地读取参数。它允许你指定不同的标志和参数，并通过参数调用运行不同的代码。你可以通过使用`-h`标志来显示帮助信息或者`-s`标志来指定输出信息的样式。

下面是一个简单的示例，演示了如何使用`getopts`来读取命令行参数，并根据参数的不同运行不同的代码。

```Bash
#!/bin/bash

while getopts "hs" option; do 
    case "${option}" in
        h) echo "帮助信息：使用-h标志来查看帮助信息。";;
        s) echo "输出样式：使用-s标志来指定输出信息的样式。";;
        *) echo "未知的选项，请使用-h标志来查看帮助信息。";;
    esac
done
```

当你运行这个脚本时，可以通过在命令行中添加不同的标志来改变输出信息。例如，`bash script.sh -h`会输出帮助信息，`bash script.sh -s`会输出样式信息，而`bash script.sh -x`则会显示未知选项的提示。

## 深入探讨命令行参数

读取命令行参数在Bash编程中是非常重要的。它可以帮助你更加灵活地编写脚本，并且可以通过添加不同的标志来操作代码，从而提高代码的可读性和可扩展性。除了`getopts`指令之外，你还可以通过使用`$OPTARG`来访问带有数值或字符串的标志参数。无论你使用哪种方式，阅读命令行参数都是一个必要的技能，能够使你的Bash编程更加高效。

## 参考资料

- [Bash官方文档](https://www.gnu.org/software/bash/manual/bash.html)
- [理解Bash中的命令行参数](https://www.howtogeek.com/442908/how-to-understand-a-programming-languages-command-line-arguments/)
- [Bash中的getopts指令](https://www.baeldung.com/linux/bash-getopts)
- [通过例子学习Bash编程](https://blog.mattbrock.co.uk/learning-linux-3-writing-a-bash-script/)