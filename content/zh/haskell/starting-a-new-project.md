---
title:    "Haskell: 开始一个新项目"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 为什么要开始一个新项目

开始一个新的编程项目是一件充满创造力和乐趣的事情。它可以带来新的挑战、刺激和成就感。无论你是新手还是资深程序员，开始一个新项目都是一个学习和提升自己技能的好机会。

## 如何开始一个新项目

在Haskell中开始一个新项目非常简单。首先，我们需要安装GHC编译器和Haskell平台。然后，我们可以使用Stack来创建一个新的haskell项目。

````haskell
$ stack new myproject
````

这将创建一个包含有默认项目结构的文件夹，并生成一个简单的Haskell程序。我们可以在该文件夹中打开命令行终端，使用以下命令编译和运行程序。

````haskell
$ stack build
$ stack exec myproject
````

现在我们已经有了一个新的Haskell项目，可以开始编写代码了。

## 深入了解开始一个新项目

除了使用Stack创建项目，我们还可以手动创建一个项目文件夹，并使用Cabal来管理依赖和构建。此外，我们可以学习如何使用不同的包管理工具，如Hackage和Stackage，来扩展项目的功能。

另外，我们也可以探索使用不同的编译器、编辑器和IDE来编写Haskell代码的不同方式。无论是命令行编译还是图形化IDE，Haskell都拥有广泛的工具生态系统，让我们可以选择最适合自己的工具来开发项目。

# 参考链接

- [Haskell官方网站](https://www.haskell.org/)
- [Haskell学习资源列表](https://github.com/bitemyapp/learnhaskell)
- [Stack官方文档](https://docs.haskellstack.org/en/stable/GUIDE/)