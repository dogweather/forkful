---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么和为什么？

HTTP 基本认证是一种规定客户提供用户名和密码进行认证的方法。程序员使用它来授权或限制对特定资源的访问。

## 如何做：

以下是一个简单的使用Go发送带有基本认证的HTTP请求的示例：

```Go
package main

import (
    "fmt"
    "net/http"
    "encoding/base64"
)

func main() {
    client := &http.Client{}
    req, _ := http.NewRequest("GET", "https://example.com", nil)
    basicAuth := "username:password"
    auth := "Basic " + base64.StdEncoding.EncodeToString([]byte(basicAuth))
    req.Header.Add("Authorization", auth)
    resp, _ := client.Do(req)
    fmt.Println(resp.Status)
}
```

当你运行这段代码的时候，你可能会在控制台看到如下的输出（如果从 https://example.com 获取成功的话）：

```
200 OK
```

## 深入挖掘

HTTP基本认证可以回溯到最初的HTTP协议，在RFC 1945（HTTP/1.0）和RFC 2617中定义。虽然这个认证方法较为古老和简单，但在某些应用场景下依然适用。

有一些代替方法可以用来进行HTTP认证，比如摘要认证、OAuth、OpenID Connect等。基本认证最大的问题在于，虽然用户名和密码是经过Base64编码的，但在网络上传输的时候并未加密，这可能导致安全性问题。

在上述Go代码中，我们使用Go基础库net/http中的Client和Request对象来构建和发送HTTP请求。将用户名和密码以"username:password"的形式用base64编码后，添加到了HTTP请求头的"Authorization"字段中。

## 另请参阅

* Go net/http文档：https://golang.org/pkg/net/http/
* RFC 1945（HTTP/1.0）：https://tools.ietf.org/html/rfc1945
* RFC 2617（HTTP认证）：https://tools.ietf.org/html/rfc2617