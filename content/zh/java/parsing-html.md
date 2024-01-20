---
title:                "解析HTML"
date:                  2024-01-20T15:32:18.560113-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/parsing-html.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？ (What & Why?)
解析HTML是指让程序能理解和操纵网页的HTML代码的过程。程序员这样做是为了提取数据、自动化测试或者修改网页内容。

## 如何操作： (How to:)
在Java中，我们通常使用Jsoup库来解析HTML。先引入库：

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
```

然后是简单的使用示例：

```Java
public class HtmlParserExample {
    public static void main(String[] args) {
        String html = "<html><head><title>First parse</title></head>"
                    + "<body><p>Parsed HTML into a doc.</p></body></html>";
        Document doc = Jsoup.parse(html);
        System.out.println(doc.title());
        Elements paragraphs = doc.select("p");
        for (Element paragraph : paragraphs) {
            System.out.println(paragraph.text());
        }
    }
}
```

输出将会是：

```
First parse
Parsed HTML into a doc.
```

## 深入了解： (Deep Dive)
HTML解析已经存在很长时间了，因为网页几乎是互联网的基础。Java中，除了Jsoup之外，还有HtmlUnit、TagSoup等库。Jsoup的优势在于它的API易用性强，并且对现代HTML的支持很好。它使用DOM（文档对象模型）分析和提取HTML元素，这意味着你可以像操作JavaScript中的DOM一样操作HTML元素。

## 参见： (See Also)
- Jsoup官网：[Jsoup](https://jsoup.org/)
- HtmlUnit官网：[HtmlUnit](http://htmlunit.sourceforge.net/)
- W3C HTML DOM教程：[W3Schools HTML DOM](https://www.w3schools.com/js/js_htmldom.asp)