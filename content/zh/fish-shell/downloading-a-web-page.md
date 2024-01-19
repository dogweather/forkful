---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

简单来说，下载网页就是把网页的所有内容（包括文本、图片和代码等）保存到你的机器上。程序员下载网页主要进行页面分析，信息抓取或者进行离线阅读。

## 如何操作：

让我们看看如何在 Fish Shell 中下载一个网页。可以使用 curl 命令，如下就是一个例子：

```Fish Shell
curl https://www.example.com > example.html
```

以上命令将网页内容下载并保存为 example.html。

## 深入探索：

1. 历史背景：自互联网出现以来，程序员就开始通过各种方法获取和保存网页内容。于是，下载网页就成为一项常见任务。
   
2. 可选方案：除了使用 curl，你也可以使用 wget 命令。并且，其他编程语言如 Python，Java 等也提供了下载网页的函数库。
   
3. 实现细节：下载网页实际上是一个 HTTP GET 请求的过程。服务器返回的响应包含网页的所有内容。

## 参考链接：

1. Fish Shell 官方文档：https://fishshell.com/docs/current/index.html
2. curl 命令详细教程：https://everythingcurl.com/
3. HTTP GET 请求了解更多: https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods/GET