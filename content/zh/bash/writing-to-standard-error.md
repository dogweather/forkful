---
title:                "Bash: 使用标准错误写作"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

在编写Bash脚本时，标准错误（standard error）是一个重要的概念。它表示程序执行时的错误信息，通过将错误信息输出到标准错误流，我们可以更容易地调试我们的程序。在本篇文章中，我们将深入探讨如何有效地编写到标准错误。

## 如何做到

首先，让我们来看一个简单的例子。假设我们有一个名为“test.sh”的Bash脚本，其中包含以下内容：

```Bash
echo "Hello World!"
echo "This is a test"
```

如果我们运行这个脚本，我们会得到以下输出：

```
Hello World!
This is a test
```

但是，如果我们在第二行加入一个错误，比如将"echo"拼写为"ehco"，我们会得到以下输出：

```
Hello World!
test.sh: line 2: ehco: command not found
This is a test
```

可以看到，错误信息被输出到了标准错误流，并且指明了出错的位置。这样，我们就可以更容易地找到并纠正我们的错误。

除了像上面这样在脚本中手动指定错误信息输出到标准错误流外，我们也可以使用“>&2”来将错误信息重定向到标准错误流。例如：

```Bash
cat test.txt &>2
```

这条命令将会读取一个名为“test.txt”的文件并将所有错误信息输出到标准错误流中。

## 深入探讨

标准错误流除了在调试程序时非常有用外，它还可以帮助我们将脚本的输出与错误信息分离开来，从而更容易地处理和跟踪错误。我们也可以通过使用特殊的错语编码来表明不同类型的错误。例如，使用“1”表示一般错误，“2”表示语法错误，“3”表示文件不存在等等。这样，我们就可以根据不同的错误码来决定程序应该如何处理错误。

同时，在编写脚本时，我们也可以使用“trap”命令来捕获并处理错误信息。例如，我们可以在脚本的开头加入以下代码，让程序发生错误时打印出错误信息并终止程序的执行：

```Bash
function error_handler() {
    echo "An error occurred in line $LINENO: $1" >&2
    exit 1
}

trap 'error_handler $( funcstack ), bash_command "</dev/stderr>’ ERR
```

这段代码将错误信息打印到标准错误流，并使用“trap”命令来捕获错误信息并执行自定义的错误处理函数。

## 参考资料

- 更多关于标准错误流的使用方法 - https://www.linuxjournal.com/article/4926
- 理解Bash错误处理 - https://www.tecmint.com/understand-linux-standard-error-redirect-and-trapping-errors/
- 异常处理和错误码 - https://stackoverflow.com/questions/17281517/custom-error-codes-in-bash-script

## 参见

- 编写可靠的Bash脚本 - https://linuxhint.com/writing_reliable_bash_scripts/
- 常用的Bash内置命令 - https://devhints.io/bash
- Bash中文文档 - https://xen-orchestra.com/blog/bash-for-beginners/