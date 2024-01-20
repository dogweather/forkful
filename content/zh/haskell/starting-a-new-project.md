---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么？

启动新项目是程序员编写新一轮程序以解决新问题或实现新功能的过程。程序员之所以这样做是因为他们需要创造出新的代码去满足不断变化的需求。

## 如何进行：

在Haskell中，我们可以从一个简陋的“Hello, World!”开始新项目。就像这样：

```Haskell
main :: IO ()
main = putStrLn "Hello, World!"
```
如果你运行这段代码，它将输出字符串 `Hello, World!`。

## 深入理解

Haskell由〖Miranda〗的启发，并在1987年开始开发，并于1990发布。想要深入了解Haskell是如何工作的，你需要理解它是如何处理并发和IO的。至于Haskell的替代方案，Erlang、Clojure和Scala等函数式编程语言可能是可行的选项。

Haskell项目一般会通过Stack或Cabal等构建工具来启动。以下是使用Stack启动新Haskell项目的方法：

1.安装Stack。

```
curl -sSL https://get.haskellstack.org/ | sh
```

2.创建新项目

```
stack new my_project
```

## 相关资源

- Haskell官方网站：[https://www.haskell.org/](https://www.haskell.org/)
- Stack构建工具：[https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)
- Haskell语言报告: [https://www.haskell.org/onlinereport/haskell2010/](https://www.haskell.org/onlinereport/haskell2010/)
- 开源书籍《Learn You a Haskell for Great Good!》：[http://learnyouahaskell.com/](http://learnyouahaskell.com/)