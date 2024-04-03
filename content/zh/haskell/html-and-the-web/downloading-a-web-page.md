---
date: 2024-01-20 17:44:22.329829-07:00
description: "\u600E\u4E48\u505A\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.814195-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
