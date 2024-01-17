---
title:                "下载网页"
html_title:           "Haskell: 下载网页"
simple_title:         "下载网页"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 什么是下载网页？
下载网页是指从互联网上获取网页文件并存储到本地计算机的过程。程序员们通常会需要下载网页，因为它可以让他们获得网络数据并运用在他们的编程项目中。

# 如何进行？
下面是一个简单的Haskell代码示例，展示了如何下载一个网页并获取其中的标题和正文内容：
```
import Network.HTTP
import Network.Stream

main = do
  page <- simpleHTTP (getRequest "https://www.example.com") >>= getResponseBody
  let title = takeWhile (/= '<') (tail (dropWhile (/= '<') (tail (dropWhile (/= '<') page))))
  let body = takeWhile (/= '<') (tail (dropWhile (/= '<') (tail (dropWhile (/= '<') (tail (dropWhile (/= '<') page))))))
  putStrLn ("Title: " ++ title)
  putStrLn ("Body: " ++ body)
```
这段代码会从https://www.example.com下载网页，并打印出网页的标题和正文内容。

# 深入了解
下载网页在历史上起着重要作用，它可以让人们从网络上获取信息并进行各种数据处理。除了使用Haskell提供的API外，还可以使用其他编程语言（如Python或Java）来实现网页下载功能。

# 参考链接
- [Haskell的Network.HTTP模块文档](https://hackage.haskell.org/package/HTTP)
- [关于网页下载的更多信息](https://en.wikipedia.org/wiki/Web_scraping)