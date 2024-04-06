---
date: 2024-01-20 18:01:28.706237-07:00
description: "How to: \u6DF1\u5165\u4E86\u89E3\uFF1A \u5728\u5386\u53F2\u4E0A\uFF0C\
  \u57FA\u672C\u8BA4\u8BC1\u662FHTTP\u534F\u8BAE\u65E9\u671F\u7528\u4E8E\u9A8C\u8BC1\
  \u7528\u6237\u8EAB\u4EFD\u7684\u7B80\u5355\u65B9\u5F0F\u3002\u5982\u4ECA\uFF0C\u867D\
  \u7136\u5B58\u5728\u66F4\u5B89\u5168\u7684\u66FF\u4EE3\u65B9\u6848\uFF08\u5982OAuth2\u548C\
  JWT\uFF09\uFF0C\u4F46\u57FA\u672C\u8BA4\u8BC1\u56E0\u5176\u7B80\u5355\u6027\uFF0C\
  \u5728\u67D0\u4E9B\u60C5\u51B5\u4E0B\u4ECD\u88AB\u4F7F\u7528\uFF0C\u7279\u522B\u662F\
  \u5728\u5185\u90E8\u7CFB\u7EDF\u6216\u5C11\u91CF\u7528\u6237\u7684\u7CFB\u7EDF\u4E2D\
  \u3002\u5B9E\u73B0\u65F6\uFF0C\u91CD\u8981\u7684\u662F\u5BF9\u51ED\u8BC1\u8FDB\u884C\
  \u7F16\u7801\uFF0C\u5E76\u5728HTTP\u8BF7\u6C42\u7684\u5934\u90E8\u53D1\u9001\u3002\
  Elm\u4E2D\uFF0C\u5229\u7528\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.990040-06:00'
model: gpt-4-1106-preview
summary: "\u6DF1\u5165\u4E86\u89E3\uFF1A \u5728\u5386\u53F2\u4E0A\uFF0C\u57FA\u672C\
  \u8BA4\u8BC1\u662FHTTP\u534F\u8BAE\u65E9\u671F\u7528\u4E8E\u9A8C\u8BC1\u7528\u6237\
  \u8EAB\u4EFD\u7684\u7B80\u5355\u65B9\u5F0F\u3002\u5982\u4ECA\uFF0C\u867D\u7136\u5B58\
  \u5728\u66F4\u5B89\u5168\u7684\u66FF\u4EE3\u65B9\u6848\uFF08\u5982OAuth2\u548CJWT\uFF09\
  \uFF0C\u4F46\u57FA\u672C\u8BA4\u8BC1\u56E0\u5176\u7B80\u5355\u6027\uFF0C\u5728\u67D0\
  \u4E9B\u60C5\u51B5\u4E0B\u4ECD\u88AB\u4F7F\u7528\uFF0C\u7279\u522B\u662F\u5728\u5185\
  \u90E8\u7CFB\u7EDF\u6216\u5C11\u91CF\u7528\u6237\u7684\u7CFB\u7EDF\u4E2D\u3002\u5B9E\
  \u73B0\u65F6\uFF0C\u91CD\u8981\u7684\u662F\u5BF9\u51ED\u8BC1\u8FDB\u884C\u7F16\u7801\
  \uFF0C\u5E76\u5728HTTP\u8BF7\u6C42\u7684\u5934\u90E8\u53D1\u9001\u3002Elm\u4E2D\uFF0C\
  \u5229\u7528 `Http` \u5305\u548C `Base64` \u7F16\u7801\u6765\u5B8C\u6210\u8FD9\u4E2A\
  \u8FC7\u7A0B\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## How to:
如何操作：
```Elm
import Http
import Base64

-- Define your endpoint and credentials
endpoint : String
endpoint = "https://your.api/endpoint"

username : String
username = "yourUsername"

password : String
password = "yourPassword"

-- Create the Basic Auth header
basicAuthHeader : Http.Header
basicAuthHeader =
    let
        encodedCredentials =
            Base64.encode (username ++ ":" ++ password)
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- Make the HTTP request with Basic Authentication
makeRequest : Http.Request String
makeRequest =
    Http.request
        { method = "GET"
        , headers = [ basicAuthHeader ]
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectString (Result.withDefault "Request failed")
        , timeout = Nothing
        , tracker = Nothing
        }
```

## Deep Dive:
深入了解：
在历史上，基本认证是HTTP协议早期用于验证用户身份的简单方式。如今，虽然存在更安全的替代方案（如OAuth2和JWT），但基本认证因其简单性，在某些情况下仍被使用，特别是在内部系统或少量用户的系统中。实现时，重要的是对凭证进行编码，并在HTTP请求的头部发送。Elm中，利用 `Http` 包和 `Base64` 编码来完成这个过程。

## See Also:
参见：
- Elm HTTP package documentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm Base64 package documentation: [https://package.elm-lang.org/packages/truqu/elm-base64/latest/](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
- HTTP Basic Auth standard: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
