---
title:                "打印调试输出"
html_title:           "Bash: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么

当我们在编写Bash脚本时，经常会遇到一些错误和问题，而输出调试信息是一种快速有效的方法来找出问题所在。通过打印调试输出，我们可以查看变量的值、函数的执行顺序以及条件语句的分支情况，从而更容易定位和解决问题。

## 如何

在Bash中，我们可以使用`echo`命令来输出调试信息。下面是一个例子：

```Bash
#!/bin/bash

# 定义一个函数来计算两个数的和
add() {
  echo "计算开始"
  echo "第一个数为 $1"
  echo "第二个数为 $2"
  sum=$(($1 + $2))
  echo "计算结果为 $sum"
}

# 调用函数并传入两个参数来进行计算
add 5 7
```

运行以上脚本后，我们会得到以下的输出：

```
计算开始
第一个数为 5
第二个数为 7
计算结果为 12
```

通过打印调试输出，我们可以清楚地看到函数内的变量值和执行顺序，从而更容易理解程序的运行过程。除了使用`echo`命令外，我们还可以使用`set -x`命令来开启调试模式，让Bash在执行完每一行命令后自动输出它的命令名和对应的参数，以帮助我们更好地调试。

## 深入了解

除了`echo`命令和`set -x`命令外，Bash还提供了许多其他的调试方法，比如调用`trap`函数来设置一个陷阱，当脚本出现错误时，自动打印出错的文件名和行号；使用`/dev/stderr`来将调试信息输出到标准错误流中，从而不影响标准输出流的内容；以及使用`eval`函数来动态执行一段字符串命令，以方便在调试过程中修改和执行命令。想要深入了解这些调试方法，可以参考下面的链接。

## 参考链接

- [Bash Debugging Tips and Tricks](https://vaneyckt.io/posts/bash_debugging_tips_and_tricks/)
- [Debugging Bash scripts](https://linuxconfig.org/debugging-bash-scripts)
- [Bash Shell Debugging Techniques](https://www.baeldung.com/linux/bash-shell-debugging-techniques)