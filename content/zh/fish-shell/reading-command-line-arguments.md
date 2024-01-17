---
title:                "阅读命令行参数"
html_title:           "Fish Shell: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么？为什么？

读取命令行参数是指程序在执行时，可以从命令行获取额外的信息。程序员这样做主要是为了让程序更加灵活和可定制。通过读取命令行参数，程序可以根据不同的输入，执行不同的任务，满足不同用户的需求。

## 怎么做？

```Fish Shell``` 提供了一个内置的函数 ```argparse```来帮助我们读取命令行参数。下面是一个简单的例子：

**代码：**

```
argparse name -h help-message
```

**输出：**

```
help-message
```

通过上面的代码，我们可以看到，程序会输出我们定义的 `help-message`，这样就可以帮助用户了解到我们的程序需要哪些参数。

## 深入了解

### 历史背景

在早期的操作系统中，程序是通过交互式的方式执行的，即用户通过输入指令来操作程序。随着技术的发展，出现了命令行界面，使得用户可以通过命令行来执行程序。而读取命令行参数就是为了更进一步提高程序的灵活性和可定制性。

### 其他选择

除了 ```Fish Shell``` 的 ```argparse``` 函数，我们也可以使用其他工具来读取命令行参数，比如 ```getopt```。这些工具都具有类似的功能，但是在使用上可能会稍有差异。

### 实现细节

在内部，```Fish Shell``` 的 ```argparse``` 函数通过解析命令行参数后，将其存储为一个关联数组，程序可以通过调用这个数组来读取不同的参数。

## 参考资料

- [Fish Shell文档](https://fishshell.com/docs/current/cmds/argparse.html)
- [互动式系统的历史](https://en.wikipedia.org/wiki/Interactive_system)
- [getopt文档](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html)