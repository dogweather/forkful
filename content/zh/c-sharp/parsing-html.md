---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# 解析HTML: 如何、何以及何处? （Parsing HTML: The How, Why, and Where）

## 何是何以？ (What & Why?)
解析HTML就是将HTML代码转化为更易于处理的数据格式。程序员通常这样做以便任意操纵Web数据。

## 如何 (How to:)
现在让我们一起介绍一种方法来解析HTML。我们将使用HtmlAgilityPack库。

安装HtmlAgilityPack库：
```C#
PM> Install-Package HtmlAgilityPack
```

通过以下样例代码对HTML进行解析：
```C#
using HtmlAgilityPack;

var web = new HtmlWeb();
var doc = web.Load("http://yoursite.com");

var nodes = doc.DocumentNode.SelectNodes("//a[@href]");

Console.WriteLine("Found: " + nodes.Count() + " nodes.");
```

这里我们找出了页面上的所有超链接。输出可能如下：
```C#
Found: 15 nodes.
```

## 深入了解 (Deep Dive)
解析HTML的概念已经有很长一段历史了。实际上，第一个网页爬虫（也就是HTML解析器的早期形式）在世界茨导网的初期就已经诞生。

除了HtmlAgilityPack之外，C#还有其他一些HTML解析库，比如AngleSharp和CsQuery。每个库都有它们自己的优点，选择哪个主要取决于您的项目需求。

实现上，最大的挑战在于处理那些不规范或者破碎的HTML。大多数现代HTML解析库都已经采用了错误修复机制来处理这个问题。

## 更多信息 (See Also)
以下是一些关于C# HTML解析的相关资源：
1. [HtmlAgilityPack库官方文档](https://html-agility-pack.net/)
2. [探索 AngleSharp](https://anglesharp.github.io/)
3. [CsQuery库的Github页面](https://github.com/jamietre/CsQuery)
4. [StackOverflow: 解析HTML的最佳方法是什么?](https://stackoverflow.com/questions/81991/a-potentially-dangerous-request-form-value-was-detected-from-the-client)