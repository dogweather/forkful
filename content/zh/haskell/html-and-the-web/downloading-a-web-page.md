---
date: 2024-01-20 17:44:22.329829-07:00
description: "\u600E\u4E48\u505A\uFF1A \u4E0B\u8F7D\u7F51\u9875\u662F\u4E2A\u8001\u65E9\
  \u5C31\u6709\u7684\u9700\u6C42\uFF0C`curl`\u548C`wget`\u662F\u65E9\u671F\u5E38\u7528\
  \u5DE5\u5177\u3002Haskell \u793E\u533A\u4E3A\u6B64\u5F00\u53D1\u4E86\u5F88\u591A\
  \u5E93\uFF0C\u50CF\u662F`http-conduit`\uFF0C\u8FD8\u6709`wreq`\uFF0C`req`\u7B49\u3002\
  `http-conduit`\u4F7F\u7528\u4E86\u7BA1\u9053\u6A21\u5F0F\uFF0C\u9002\u5408\u5904\
  \u7406\u6D41\u6570\u636E\u3002\u6267\u884C\u6548\u7387\u9AD8\uFF0C\u4F46\u5B66\u4E60\
  \u66F2\u7EBF\u53EF\u80FD\u9661\u5CED\u4E9B\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.976290-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A\uFF1A \u4E0B\u8F7D\u7F51\u9875\u662F\u4E2A\u8001\u65E9\
  \u5C31\u6709\u7684\u9700\u6C42\uFF0C`curl`\u548C`wget`\u662F\u65E9\u671F\u5E38\u7528\
  \u5DE5\u5177\u3002Haskell \u793E\u533A\u4E3A\u6B64\u5F00\u53D1\u4E86\u5F88\u591A\
  \u5E93\uFF0C\u50CF\u662F`http-conduit`\uFF0C\u8FD8\u6709`wreq`\uFF0C`req`\u7B49\u3002\
  `http-conduit`\u4F7F\u7528\u4E86\u7BA1\u9053\u6A21\u5F0F\uFF0C\u9002\u5408\u5904\
  \u7406\u6D41\u6570\u636E\u3002\u6267\u884C\u6548\u7387\u9AD8\uFF0C\u4F46\u5B66\u4E60\
  \u66F2\u7EBF\u53EF\u80FD\u9661\u5CED\u4E9B\u3002"
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
