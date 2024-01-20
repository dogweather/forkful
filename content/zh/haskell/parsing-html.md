---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## 什么和为什么?

解析HTML是处理HTML源代码以提取数据或信息的过程。程序员之所以要进行HTML解析，主要是为了从网页上抽取有用的信息。

## 怎么做:

使用Haskell来解析HTML, 我们将采用`tagsoup`库，它销售一个简洁的API，很适合我们的需求。

让我们下载这个库并写个例子:

```Haskell
import Text.HTML.TagSoup

main = do
    let tags = parseTags "<html><body><a href=www.google.com>Google!</a></body></html>"
    print $ filter (~== "<a>") tags
```

上述代码解析HTML字符串，然后筛选出所有的`<a>`标签。

运行结果应该如下:

```Haskell
[TagOpen "a" [("href","www.google.com")],TagText "Google!",TagClose "a"]
```

## 深度解析

历史背景: Haskell的`tagsoup`库由Neil Mitchell创建，他是一位知名的Haskell程序员。这个库鼓励使用函数式编程的方式处理HTML，它相比传统的暴力解析方法，更简洁并且强大。

替代方案: `tagsoup`不是唯一的选择，还有`html-conduit`、`hxt`、`lambdasoup`等库也可以实现HTML的解析。

实施细节: `tagsoup`通过标记化HTML，然后使用模特匹配（pattern matching）的方式，对HTML元素进行筛选和提取。

## 扩展阅读

如果你对解析HTML更感兴趣，你可以查看以下链接:

1. [TagSoup库官方文档](https://hackage.haskell.org/package/tagsoup)
2. [Neil Mitchell的博客](http://neilmitchell.blogspot.com/)