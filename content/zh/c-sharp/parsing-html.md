---
title:                "C#: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

今天我们来谈谈在C#中如何解析HTML。HTML是一种用于创建网页的标记语言，解析它可以让我们从网页中提取出需要的信息。比如，我们可以通过解析HTML来获取某个网站的标题、文本内容、图片等信息。在这篇文章中，我们将学习如何使用C#来解析HTML，并且将深入了解HTML解析的更多细节。

## 为什么要解析HTML

解析HTML可以让我们从网页中提取出需要的信息，并将其用于数据分析、网页抓取等应用。在很多Web开发和数据科学领域，解析HTML都是非常常见的操作。通过学习如何解析HTML，我们可以更加灵活地处理网页数据，从中挖掘出有价值的信息。

## 如何解析HTML

下面我们将使用C#来解析HTML，并获取其中的文本内容和图片链接。首先，我们需要引入HtmlAgilityPack包，它是一个常用的HTML解析器。然后，我们可以通过以下代码来解析HTML：

```C#
// 引入HtmlAgilityPack包
using HtmlAgilityPack;

// 创建HtmlDocument对象
var doc = new HtmlDocument();

// 加载HTML文件
doc.Load("sample.html");

// 使用XPath获取h1标签中的文本
var title = doc.DocumentNode.SelectSingleNode("//h1").InnerText;

// 使用XPath获取所有p标签中的文本
var paragraphs = doc.DocumentNode.SelectNodes("//p");

// 使用foreach循环输出p标签中的文本
foreach (var p in paragraphs)
{
    Console.WriteLine(p.InnerText);
}

// 使用CssSelector获取第一个img标签中的src属性
var imgSrc = doc.DocumentNode.SelectSingleNode("img:first-of-type").GetAttributeValue("src", "no-src");

// 输出结果
Console.WriteLine("标题：" + title);
Console.WriteLine("第一段文本：" + paragraphs[0].InnerText);
Console.WriteLine("第二段文本：" + paragraphs[1].InnerText);
Console.WriteLine("图片链接：" + imgSrc);
```

运行以上代码，我们可以得到以下输出：

```
标题：Hello World!
第一段文本：欢迎来到我的博客。
第二段文本：我是一名程序员，喜欢写博客分享技术和经验。
图片链接：https://myblog.com/img/logo.png
```

## 深入了解HTML解析

除了使用XPath和CssSelector来提取文本和属性外，我们还可以使用HtmlDocument对象提供的其他方法来处理HTML。例如，我们可以通过HtmlDocument对象的`CreateNavigator`方法来创建一个XPathNavigator对象，然后使用它来更加灵活地定位和提取HTML中的元素。

同时，HtmlAgilityPack还支持解析含有无效或不规范HTML的网页，它可以自动修复HTML并生成有效的DOM树。这对于大部分实际应用场景是非常必要的。

## 参考链接

- [HtmlAgilityPack官方文档](https://html-agility-pack.net/documentation)
- [C#中解析HTML的几种方法](https://www.cnblogs.com/tianma3798/p/11312904.html)

## 参见

见长链接：

- [C#中使用正则表达式的基本方法](https://github.com/jason2017/myblog/blob/master/csharp-regex.md)
- [C#中使用LINQ进行数据处理](https://github.com/jason2017/myblog/blob/master/csharp-linq.md)