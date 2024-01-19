---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么(What) & 为什么(Why)?
下载网页是一种获取网页源代码的过程，程序员主要用来进行数据提取、网站爬取和自动测试。

## 如何实现(How to):
In Haskell, 我们可以通过 `http-conduit` 库来下载网页。以下是一个简单的例子:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
    putStrLn "请输入需要下载的网页链接:"
    url <- getLine
    pageSource <- simpleHttp url
    putStrLn $ show pageSource
```
当你运行这段代码，程序会要求你输入一个网页链接，然后程序会返回网页的源代码。

## 深入探索 (Deep Dive):

1. 历史背景: Haskell 在1990年就提出`IO Monad`的概念，一直沿用至今。用于处理所有形式的输入/输出(IO)，包括网络请求。
   
2. 可选方案：如果考虑实现更复杂的爬虫，可能会考虑使用`http-client`库，它允许更灵活的配置，并提供了许多额外的特性，如 Cookie Support 和代理设置。

3. 实现细节: `simpleHttp`函数使用底层的`http-client`库，发送`GET`请求并将结果作为一个`ByteString`返回。需要注意的是，这种方法会直接读入整个response到内存，如果你预计返回的数据量十分大，你可能需要使用更底层的`http-client`接口。

## 参看 (See Also):

1. `http-conduit`库的Hackage页面: [http://hackage.haskell.org/package/http-conduit](http://hackage.haskell.org/package/http-conduit)
2. `Wreq`库的Github页面: [https://github.com/bos/wreq](https://github.com/bos/wreq)
3. Haskell 官方文档关于`IO Monad`的部分: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)