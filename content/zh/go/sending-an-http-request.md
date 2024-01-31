---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:38.345815-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
发送HTTP请求就是让你的程序能向服务器问问题或者发送信息。程序员这么做通常是为了获取数据，更新数据，或者和其他服务交互。

## How to: (怎么做：)
```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    response, err := http.Get("http://example.com")
    if err != nil {
        fmt.Printf("The HTTP request failed with error %s\n", err)
    } else {
        data, _ := ioutil.ReadAll(response.Body)
        fmt.Println(string(data))
    }
}
```
样本输出：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (深入探究)
HTTP请求是万维网的基础。1990年代早期，HTTP/0.9被发明出来。现在用的是HTTP/2，HTTP/3也在路上了。发送HTTP请求有多种方式，`net/http` 是Go提供的官方库。它简洁，强大，适合多数情况。也可以用第三方库，比如 `gorequest` 和 `resty`，提供更多特性，更简便的API。但他们不是官方支持的。

掌握发送请求的细节很重要。头信息、请求方法(GET, POST等)、处理响应和错误都得考虑。比如，处理JSON，可能需要 `encoding/json` 库来解析。

## See Also (参见)
- Go net/http包文档: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- 用Go发送HTTP请求: [https://golang.org/doc/articles/wiki/](https://golang.org/doc/articles/wiki/)
- 第三方库gorequest: [https://github.com/parnurzeal/gorequest](https://github.com/parnurzeal/gorequest)
- 第三方库resty: [https://github.com/go-resty/resty](https://github.com/go-resty/resty)
