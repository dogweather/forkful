---
title:                "解析HTML"
date:                  2024-01-20T15:30:28.537725-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

category:             "C#"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

HTML解析就是把网页代码转换成可供程序使用的结构化数据。程序员通常进行解析，目的是从网页抓取数据、自动化测试或操作DOM。

## How to: (如何操作：)

C#里解析HTML很简单，我们可以用HtmlAgilityPack这个库。下面是个简单例子：

```C#
using System;
using HtmlAgilityPack;

public class HtmlParserExample
{
    public static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("http://example.com"); // 替换成你要访问的网站
        
        foreach(var para in doc.DocumentNode.SelectNodes("//p"))
        {
            Console.WriteLine(para.InnerText);
        }
    }
}
```

运行后你会看到所有<p>标签内的文本。

## Deep Dive (深入探索)

早期HTML解析是通过正则表达式等方法手动执行，但这种方式容错率低且复杂。HtmlAgilityPack是2005年左右出现的。它有强大的XPath和CSS选择器支持。替代方案有AngleSharp等，但HtmlAgilityPack因其稳定性和灵活性而广受欢迎。在使用时，了解HTML DOM结构很重要。此外，尊重robots.txt和网页版权是合法和道德的网络抓取的关键。

## See Also (另请参阅)

- HtmlAgilityPack官方文档：https://html-agility-pack.net/
- AngleSharp GitHub页面：https://github.com/AngleSharp/AngleSharp
- W3C HTML DOM教程：https://www.w3schools.com/js/js_htmldom.asp
