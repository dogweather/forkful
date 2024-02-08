---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:01:28.706237-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
什么以及为什么？
发送带有基本认证的HTTP请求意味着在请求中包含用户名和密码信息，以获取访问权限。程序员这样做是为了安全地从受保护的服务中获取数据或执行操作。

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
