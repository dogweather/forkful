---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么与为什么？
下载网页就是从互联网将网页内容下载到本地。程序员这样做，以便分析网页内容，抓取信息，或者进行离线浏览。

## 如何操作：
在Gleam中下载网页非常直观，可以使用`httpc`模块中的`send`函数。这是一个基础的示例：

```Gleam
import gleam/httpc
import gleam/http.{Method}

fn download() {
  let request = httpc.new(Method.Get, "http://example.com")
  case httpc.send(request) {
    Ok(response) -> print(response.body)
    Error(e) -> print(e)
  }
}

download()
```
运行以上代码，你会看到`http://example.com`页面的HTML内容。

## 深度挖掘
下载网页的背后有一段有趣的历史。最初的网页下载是通过telnet命令进行的，直到出现了HTTP协议，事情才变得更加简单。至于在Gleam中，我们有很多方式可以下载网页，比如使用`reqwest`， `hyper`等库。但是`httpc`是最简单，也是最直接的方式。你可以在Gleam的标准库中找到它。

## 另请参阅
-Gleam的HTTP客户端模块文档:[https://hexdocs.pm/gleam_stdlib/gleam/httpc](https://hexdocs.pm/gleam_stdlib/gleam/httpc)
-Gleam教程和示例：[https://gleam.run/tutorials/](https://gleam.run/tutorials/)
-更多关于Web抓取的信息：[https://en.wikipedia.org/wiki/Web_crawling](https://en.wikipedia.org/wiki/Web_crawling)