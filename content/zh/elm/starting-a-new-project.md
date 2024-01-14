---
title:                "Elm: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 为什么选择开始一个新的项目？

在这个快节奏的科技时代，学习和掌握新的编程语言是非常重要的。Elm作为一种功能强大且易于学习和使用的函数式编程语言，越来越受到程序员们的关注。通过开始一个新的Elm项目，你可以学习这门语言并且创建令人惊叹的Web应用程序。

# 如何开始？

首先，你需要安装Elm编译器。你可以通过下面的命令在终端中使用基于Node.js的包管理器NPM来进行安装：

```Elm
npm install -g elm
```

安装完成后，你就可以开始编写Elm代码了。下面是一个简单的Hello World程序的例子：

```Elm
module Main exposing (main)

import Html exposing (text)

main =
    text "Hello, world!"
```

在你的终端中，使用elm命令将代码编译为JavaScript，并将结果输出到index.html文件中：

```Elm
elm make Main.elm --output=index.html
```

打开index.html文件，你会看到文本“Hello, world!”显示在你的浏览器中。

# 深入了解

虽然这只是一个简单的例子，但它展示了Elm语言的一些基本特性，如模块化和声明式语法。如果你想深入了解Elm，你可以阅读官方文档或参考下面的链接来学习更多关于开始一个新的Elm项目的技巧和建议。

# 参考链接

- [Elm官方网站](https://elm-lang.org/)
- [Elm学习资源汇总](https://github.com/isRuslan/awesome-elm)
- [用Elm构建前端应用程序的5个原因](https://www.sitepoint.com/five-reasons-to-use-elm-build-front-end-apps/)
- [使用Elm与React构建现代Web应用程序](https://www.toptal.com/front-end/elm-react-sync)
- [使用Elm构建一个简单的Todo应用程序](https://medium.com/@ciricomid/elm-to-do-app-666f6aebf307)

# 其他

# 参见

- [Elm官方文档](https://guide.elm-lang.org/)
- [Elm入门教程](https://www.elm-tutorial.org/zh-cn/)
- [Elm论坛](https://discourse.elm-lang.org/)