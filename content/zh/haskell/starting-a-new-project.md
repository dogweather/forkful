---
title:                "开始一个新的项目"
html_title:           "Haskell: 开始一个新的项目"
simple_title:         "开始一个新的项目"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

What & Why?

在程序员的世界里，开始一个新的项目是非常常见的。这意味着创建一个全新的软件或者为现有的项目添加新的特性。程序员通常会开始一个新的项目，因为他们想要尝试新的技术、解决新的问题或者简单地寻找一些新的挑战。

How to:

```Haskell
-- 创建一个新的项目
stack new my-project
cd my-project

-- 编译和运行你的项目
stack build
stack exec my-project-exe
```

Deep Dive:

在过去，程序员可能会手动创建他们的项目文件和目录，然后手动编译和运行他们的代码。但是现在，有许多工具和框架可以帮助我们更高效地开始一个新的项目。例如，在Haskell中，我们可以使用Stack创建一个新的项目，它会自动为我们设置好项目结构和依赖，让我们可以专注于编写代码。当然，还有其他的选择，比如使用Haskell的原生工具Cabai、Cabal-sandbox或者Nix。

See Also:

- [Haskell Stack 用户手册](https://docs.haskellstack.org/en/stable/README/)
- [Haskell Cabal 用户手册](https://www.haskell.org/cabal/users-guide/index.html)
- [Haskell Nix 用户手册](https://nixos.org/nixpkgs/manual/#users-guide)