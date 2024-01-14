---
title:                "Gleam: 开始一个新项目 (Kāishǐ yī gè xīn xiàngmù)"
simple_title:         "开始一个新项目 (Kāishǐ yī gè xīn xiàngmù)"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 为什么要开始一个新项目？

开始一个新项目可以带来许多好处。它可以是一个学习新技能的机会，也可以是实现自己想法的途径。无论是作为个人兴趣还是商业需求，开始一个新项目都能激发我们的创造力和发挥最大潜力。

## 如何开始？

首先，您需要安装Gleam编程语言。它是一个功能强大的静态类型语言，专门为分布式系统和并发编程设计。您可以在命令行中输入`brew install gleam`（适用于Mac用户）或`npm install -g gleam`（适用于Windows用户）来安装Gleam。

接下来，让我们来看一个简单的例子。我们将创建一个打印“Hello, World！”的简单程序。

```
Gleam
pub fn main() {
    io.print("Hello, World!")
}
```

在上面的代码中，我们使用了Gleam内置的`io`模块，并调用了其中的`print`函数来打印我们想要输出的内容。要运行这段代码，您可以使用`gleam run hello_world.gleam`命令，将文件名替换为您自己的文件名。

## 深入探讨

当开始一个新项目时，这里有一些课题值得深入探讨。首先，您需要确定您的项目的目的和范围。这将帮助您选择合适的编程语言和工具。其次，您需要规划好项目的结构和架构，以便未来的扩展和维护。最后，您还需要学习如何使用不同的框架和库来实现您的想法。

另外，Gleam拥有活跃的社区，您可以通过加入Slack群组、参加线下活动和阅读官方文档来深入了解这门语言。您也可以参考一些开源项目的源代码，来学习如何在实践中使用Gleam。

# 参考链接

- [官方文档](https://gleam.run/)
- [Gleam语言GitHub仓库](https://github.com/gleam-lang/gleam)
- [Gleam语言Slack群组](https://join.slack.com/t/gleam-lang/shared_invite/zt-nzxcggv6-CXdP3CNvH4NlKxM66V_jBg)
- [Gleam语言中文论坛](https://learn-gleam.cn/)
- [一个使用Gleam开发的实际项目](https://github.com/gleam-lang/primal)

# 参见

- [如何学习编程语言？](https://learn-gleam.cn/t/wen-zhang-lian-jie/141)
- [学习Gleam编程的7个建议](https://dev.to/gleamlang/7-tips-for-learning-gleam-programming-language-1abg)
- [学习Gleam编程的10个免费资源](https://dev.to/gleamlang/10-free-resources-to-learn-gleam-programming-language-58jl)