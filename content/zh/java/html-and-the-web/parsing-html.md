---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:24.233585-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u4F7F\u7528Jsoup\uFF0C\
  \u4E00\u4E2A\u7528\u4E8E\u5904\u7406\u73B0\u5B9E\u4E16\u754CHTML\u7684\u4FBF\u6377\
  \u5E93\u3002\u9996\u5148\uFF0C\u6DFB\u52A0\u4F9D\u8D56\u9879\uFF1A."
lastmod: '2024-03-13T22:44:47.623181-06:00'
model: gpt-4-0125-preview
summary: "\u8BA9\u6211\u4EEC\u4F7F\u7528Jsoup\uFF0C\u4E00\u4E2A\u7528\u4E8E\u5904\u7406\
  \u73B0\u5B9E\u4E16\u754CHTML\u7684\u4FBF\u6377\u5E93\u3002\u9996\u5148\uFF0C\u6DFB\
  \u52A0\u4F9D\u8D56\u9879\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
让我们使用Jsoup，一个用于处理现实世界HTML的便捷库。首先，添加依赖项：

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

现在来到有趣的部分。以下是如何抓取一个网页的标题并打印它：

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("标题: " + title);
    }
}
```

输出：

```
标题: 示例域名
```

如何提取所有链接？

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... 在main方法或其他方法中
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("链接: " + link.attr("href"));
}
```

## 深入探讨
曾几何时，HTML被正则表达式模式所驯服，这是一种对复杂文档既容易出错又是噩梦般的方法。随着Jsoup在二零零年代末的出现，为Java提供了类似jQuery的界面来解析、遍历和操作HTML。

Jsoup并非唯一选择。还有HtmlUnit适用于支持JavaScript的完整网络应用测试，但它更加笨重和复杂。对于轻量级任务，Apache Commons Validator非常适合仅提取URLs。

在底层，Jsoup使用了DOM解析器，它将整个文档以树的形式在内存中建模。这种方法使得选择和导航HTML结构变得很容易。另外，它能够容忍杂乱的HTML，在飞行中修复问题以确保稳健的解析。

记住，当进行抓取时，始终检查网站的`robots.txt`和服务条款，以避免法律麻烦或被IP封禁。

## 另见
- Jsoup 官方文档：https://jsoup.org/
- HtmlUnit：http://htmlunit.sourceforge.net/
- Apache Commons Validator：https://commons.apache.org/proper/commons-validator/
