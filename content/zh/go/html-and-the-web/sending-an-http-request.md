---
title:                "发送HTTP请求"
aliases: - /zh/go/sending-an-http-request.md
date:                  2024-02-03T18:08:51.431521-07:00
model:                 gpt-4-0125-preview
simple_title:         "发送HTTP请求"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

发送 HTTP 请求涉及从你的 Go 应用程序向网站服务器、API 或任何其他基于 HTTP 的服务发起调用。程序员这样做是为了与网络资源进行交互、获取数据、提交表单或通过互联网与其他服务进行通信。

## 怎么做：

在 Go 中，发送 HTTP 请求并处理响应涉及使用 `net/http` 包。以下是一个分步示例，展示如何发送一个简单的 GET 请求并读取响应：

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // 定义资源的 URL
    url := "http://example.com"

    // 使用 http.Get 发送 GET 请求
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // 当函数结束时关闭响应体
    defer resp.Body.Close()

    // 读取响应体
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // 将响应体转换为字符串并打印
    fmt.Println(string(body))
}
```

示例输出（为简洁起见已缩短）：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

要发送带有表单数据的 POST 请求，你可以使用 `http.PostForm`：

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // 定义 URL 和表单数据
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // 发送带有表单数据的 POST 请求
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // 读取并打印响应
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## 深入了解

Go 的 `net/http` 包提供了一种强大且灵活的方式与 HTTP 服务器进行交互。其设计反映了 Go 对简单性、效率和稳健性的强调。最初，处理 JSON 或 XML 负载等功能需要手动构造请求体并设置适当的头部。随着 Go 的发展，社区开发了更高级的包，进一步简化了这些任务，例如 `gorilla/mux` 用于路由和 `gjson` 用于 JSON 操作。

Go 的 HTTP 客户端一个值得注意的方面是其接口和结构的使用，如 `http.Client` 和 `http.Request`，它们允许进行广泛的定制和测试。例如，你可以修改 `http.Client` 来超时请求或保持连接活跃以提高性能。

对于简化 HTTP 交互的另一种考虑是使用第三方库，如 "Resty" 或 "Gentleman"。这些包为 HTTP 请求提供了更高级的抽象，使常见任务更加简洁。然而，理解和利用底层的 `net/http` 包对于处理更复杂或独特的 HTTP 交互场景至关重要，为充分利用 Go 的并发特性和强大的标准库提供了基础。
