---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

---

## 什么和为什么?

下载网页就是将网页的内容保存在本地。程序员这么做是为了离线浏览，或者进行数据抓取和分析。

## 如何操作：

下面是一个Kotlin的下载网页的简单示例。

```Kotlin
import java.net.URL

fun main(args: Array<String>) {
    val content = URL("http://www.example.com").readText()
    println(content)
}
```

运行这份代码会显示出`www.example.com`网页的HTML内容。

## 深度探索：

**(1) 历史背景**

最初，人们手动使用浏览器保存网页，但随着编程语言的发展，如今我们通过代码来下载网页。

**(2) 选择**

除了 Kotlin，其它语言如 Python, Java 等都能做到这一点。选择哪种语言主要看你在哪种语言上能更加得心应手。

**(3) 更多细节**

我们用到的 `readText()` 方法会读取整个网页，这可能会有内存问题。如果网页很大，请考虑分块读取或使用流。

## 拓展阅读：

- Official Kotlin Documentation: https://kotlinlang.org/docs/reference/
- Handling Network Operations: https://developer.android.com/training/basics/network-ops
- BeautifulSoup (for Python users): https://www.crummy.com/software/BeautifulSoup/