---
title:                "解析HTML"
date:                  2024-01-20T15:32:34.351161-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

category:             "Kotlin"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
解析HTML指的是从HTML文档提取数据或信息。程序员这么做主要是为了自动化处理网页内容，比如抓取数据或者测试网站。

## How to (如何操作)
在Kotlin中，我们可以使用`jsoup`库来解析HTML。下面是一个简单的例子。

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = """
        <html>
            <head>
                <title>Sample Page</title>
            </head>
            <body>
                <p>This is a simple HTML document.</p>
                <p>Here's another paragraph.</p>
            </body>
        </html>
        """.trimIndent()

    val doc = Jsoup.parse(html)
    val paragraphs = doc.select("p")

    for (p in paragraphs) {
        println(p.text())
    }
}
```
这段代码会输出：
```
This is a simple HTML document.
Here's another paragraph.
```

## Deep Dive (深度解析)
解析HTML在早期主要靠正则表达式手动提取，效率低且错误多。随着技术的发展，出现了专门的库像`jsoup`，能更准确快速地处理HTML。

除了`jsoup`，还有像`htmlcleaner`、`jTidy`等库。这些库的底层实现一般都是用DOM(Document Object Model)或SAX(Simple API for XML)等解析方式，将HTML转化为可以程序化操作的对象。

选用哪个库通常基于个人偏好或特定的性能要求。比如，`jsoup`易于使用同时性能也不错，因此它非常流行。

## See Also (参考链接)
- [Jsoup 官方文档](https://jsoup.org/)
- [HTMLCleaner 项目主页](http://htmlcleaner.sourceforge.net/)
- [jTidy 的GitHub仓库](https://github.com/jtidy/jtidy)
