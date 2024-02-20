---
date: 2024-01-20 17:44:22.329829-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u5185\u5BB9\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u4E0B\u8F7D\
  \u7F51\u9875\u6765\u5206\u6790\u6570\u636E\u3001\u6D4B\u8BD5\u7F51\u7AD9\u6216\u662F\
  \u4E2A\u6027\u5316\u5185\u5BB9\u6293\u53D6\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.853134
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u5185\u5BB9\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u4E0B\u8F7D\
  \u7F51\u9875\u6765\u5206\u6790\u6570\u636E\u3001\u6D4B\u8BD5\u7F51\u7AD9\u6216\u662F\
  \u4E2A\u6027\u5316\u5185\u5BB9\u6293\u53D6\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
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
