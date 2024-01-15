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

# 为什么要下载网页？

有些时候，我们想要保存网页的内容或者想要离线浏览，这时候就需要下载网页。Haskell提供了简单易用的方法来下载网页内容，让我们一起来看看如何实现吧！

## 如何进行网页下载

首先，我们需要导入网络请求的库，它可以帮助我们发送HTTP请求以及处理响应。Haskell中有几个可用的网络请求库，本文将使用**http-conduit**作为例子。首先，我们需要安装它，我们可以使用**cabal**来安装：

```
cabal install http-conduit
```

安装完成后，让我们来编写代码。首先，我们需要导入**Network.HTTP.Conduit**模块：

```Haskell
import Network.HTTP.Conduit
```

现在，我们可以使用**simpleHttp**函数来发送一个简单的HTTP GET请求，并将响应内容保存在一个变量中：

```Haskell
myResponse <- simpleHttp "https://www.example.com"
```

响应的内容将被保存在名为**myResponse**的变量中，我们可以将它打印出来，观察网页的内容是否被成功获取：

```Haskell
print myResponse
```

这样，我们就可以完成网页的下载了。但是，如果我们想要将网页保存到本地文件中，应该怎么做呢？

## 深入了解网页下载

为了将网页保存到本地文件，我们需要使用**sinkFile**函数，它可以将网络请求的响应保存到指定的文件中。让我们来看一个完整的例子：

```Haskell
import Network.HTTP.Conduit

main :: IO ()
main = do
    myResponse <- simpleHttp "https://www.example.com"
    writeFile "example.html" myResponse
```

在这个例子中，我们首先使用**simpleHttp**函数来获取网页的内容，然后使用**writeFile**函数将内容保存到**example.html**文件中。

除了使用**sinkFile**函数，我们还可以通过**conduit**来处理响应的内容。此外，我们还可以指定请求中的header、body等内容，来实现更复杂的网页下载操作。

## 参考链接

- [Haskell官方网站](https://www.haskell.org/)
- [Haskell-http-conduit库文档](https://hackage.haskell.org/package/http-conduit)

### 另请参阅

- [Hello World的七种写法](https://www.example.com/hello-world)
- [如何安装和使用Haskell解释器](https://www.example.com/haskell-installation-guide)