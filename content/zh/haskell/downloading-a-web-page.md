---
title:                "下载网页"
aliases:
- zh/haskell/downloading-a-web-page.md
date:                  2024-01-20T17:44:22.329829-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
下载网页就是从互联网上获取网页内容的过程。程序员下载网页来分析数据、测试网站或是个性化内容抓取。

## 怎么做：
```Haskell
-- 安装HTTP套件：http-conduit
import Network.HTTP.Simple

-- 定义下载函数
downloadWebPage :: String -> IO L.ByteString
downloadWebPage url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- 使用下载函数
main :: IO ()
main = do
    content <- downloadWebPage "http://example.com"
    L.putStr content
```

示例输出：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## 深入探究
下载网页是个老早就有的需求，`curl`和`wget`是早期常用工具。Haskell 社区为此开发了很多库，像是`http-conduit`，还有`wreq`，`req`等。`http-conduit`使用了管道模式，适合处理流数据。执行效率高，但学习曲线可能陡峭些。

## 参考链接
- HTTP 协议：[https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- http-conduit 套件：[https://www.stackage.org/package/http-conduit](https://www.stackage.org/package/http-conduit)
