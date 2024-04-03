---
date: 2024-01-20 15:32:34.351161-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C) \u5728Kotlin\u4E2D\uFF0C\u6211\u4EEC\
  \u53EF\u4EE5\u4F7F\u7528`jsoup`\u5E93\u6765\u89E3\u6790HTML\u3002\u4E0B\u9762\u662F\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.717109-06:00'
model: unknown
summary: "\u5728Kotlin\u4E2D\uFF0C\u6211\u4EEC\u53EF\u4EE5\u4F7F\u7528`jsoup`\u5E93\
  \u6765\u89E3\u6790HTML\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\
  \u5B50."
title: "\u89E3\u6790HTML"
weight: 43
---

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
