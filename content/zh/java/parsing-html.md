---
title:                "解析HTML"
html_title:           "Java: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么
有时候，我们需要从网页中提取有用的信息，例如抓取新闻内容或者分析网站的数据。而这些信息通常储存在HTML代码中，因此我们需要通过解析HTML来提取所需的信息。

## 怎么做
我们来看看如何使用Java来解析HTML代码。首先，我们需要导入Jsoup库，它是一个专门用于解析HTML的Java库。

````Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
````

接下来，我们可以使用以下代码来加载HTML代码：

````Java
String html = "<html><head><title>示例网页</title></head><body><h1>Hello World!</h1><p>这是一个段落。</p></body></html>";
Document doc = Jsoup.parse(html);
````

我们可以通过调用Document对象的方法来提取所需的信息，例如获取标题：

````Java
String title = doc.title(); // 结果：示例网页
````

或者获取第一个段落的内容：

````Java
String paragraph = doc.select("p").first().text(); // 结果：这是一个段落。
````

以上就是解析HTML的基本使用方法，你可以根据自己的需求调用不同的方法来提取所需的信息。

## 深入了解
除了前面提到的基本用法，Jsoup库还提供了更多强大的功能，例如可以通过选择器来选择特定的HTML元素:

````Java
Elements paragraphs = doc.select("p"); // 所有的段落
Element firstParagraph = doc.selectFirst("p"); // 第一个段落
````

同时，我们可以通过CSS选择器来选择具有特定属性的HTML元素:

````Java
Element element = doc.selectFirst("a[href*=example.com]");
````

除了选择，Jsoup还提供了操纵和修改HTML代码的功能，例如可以将指定的元素添加到HTML代码中:

````Java
Element link = doc.selectFirst("a[id=example]");
doc.body().append(link.outerHtml());
````

通过这些更深层次的使用，我们可以灵活地解析和处理HTML代码，提取任意形式的信息。

## 参考链接
- [Jsoup官方文档](https://jsoup.org/cookbook/extracting-data/selector-syntax)
- [JREPL: 解析HTML](https://www.jrepl.com/doc/howtojsoup.html)
- [CSDN: Jsoup使用方法全解析](https://blog.csdn.net/u012373815/article/details/46834039)

## 了解更多
如果你对Java解析HTML还想了解更多，可以继续学习Jsoup的其他功能和用法，或者探索其他基于Java的HTML解析库，例如HtmlUnit和NekoHTML。