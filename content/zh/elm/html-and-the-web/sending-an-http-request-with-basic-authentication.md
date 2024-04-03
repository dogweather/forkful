---
date: 2024-01-20 18:01:28.706237-07:00
description: "\u4EC0\u4E48\u4EE5\u53CA\u4E3A\u4EC0\u4E48\uFF1F \u53D1\u9001\u5E26\u6709\
  \u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u610F\u5473\u7740\u5728\u8BF7\u6C42\
  \u4E2D\u5305\u542B\u7528\u6237\u540D\u548C\u5BC6\u7801\u4FE1\u606F\uFF0C\u4EE5\u83B7\
  \u53D6\u8BBF\u95EE\u6743\u9650\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5B89\u5168\u5730\u4ECE\u53D7\u4FDD\u62A4\u7684\u670D\u52A1\u4E2D\u83B7\u53D6\
  \u6570\u636E\u6216\u6267\u884C\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.672377-06:00'
model: gpt-4-1106-preview
summary: "\u4EC0\u4E48\u4EE5\u53CA\u4E3A\u4EC0\u4E48\uFF1F\n\u53D1\u9001\u5E26\u6709\
  \u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u610F\u5473\u7740\u5728\u8BF7\u6C42\
  \u4E2D\u5305\u542B\u7528\u6237\u540D\u548C\u5BC6\u7801\u4FE1\u606F\uFF0C\u4EE5\u83B7\
  \u53D6\u8BBF\u95EE\u6743\u9650\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5B89\u5168\u5730\u4ECE\u53D7\u4FDD\u62A4\u7684\u670D\u52A1\u4E2D\u83B7\u53D6\
  \u6570\u636E\u6216\u6267\u884C\u64CD\u4F5C\u3002."
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
