---
title:    "Fish Shell: 读取命令行参数"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

如果你是一个开发者或者系统管理员，经常需要使用命令行来操作电脑，那么了解如何读取命令行参数是非常重要的。此外，关于命令行参数的知识也可以提高你的工作效率，并且让你更加自信地使用命令行。

## 如何操作

通过下面的命令行代码块来学习如何读取命令行参数：

 ```Fish Shell
 #!/usr/bin/fish

  for arg in $argv
   echo "参数：$arg"
  end
```

命令行参数是通过使用`$argv`变量来访问的，它包含了所有传递给脚本的参数。使用`for`循环来遍历这些参数，并且通过`echo`命令来打印出来。你可以自己尝试运行以上代码，并且看一下命令行中的输出结果。

## 深入了解

除了使用`$argv`变量外，还可以使用`cmdline`函数来读取命令行参数。此外，Fish Shell还提供了一些内建的命令，比如`$PROFILE`来获取当前登录用户的配置文件路径，或者`$PWD`来获取当前工作目录路径。

如果你想对读取命令行参数有更深入的了解，可以查看Fish Shell官方文档中的相关章节。

## 查看更多

想要了解有关Fish Shell的更多知识，请参考以下链接：

- [官方文档](https://fishshell.com/docs/current/index.html)
- [命令行参数介绍](https://fishshell.com/docs/current/tutorial.html#id-section3)
- [内建变量](https://fishshell.com/docs/current/index.html#variables)
- [内建函数](https://fishshell.com/docs/current/commands.html#cmdline) 

## 参考资料

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [命令行参数介绍](https://fishshell.com/docs/current/tutorial.html#id-section3)
- [内建变量](https://fishshell.com/docs/current/index.html#variables)
- [内建函数](https://fishshell.com/docs/current/commands.html#cmdline)