---
title:                "发送一个http请求。"
html_title:           "Go: 发送一个http请求。"
simple_title:         "发送一个http请求。"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么是HTTP请求
发送HTTP请求指的是使用Hypertext Transfer Protocol (HTTP)协议向服务器发送请求，以获取特定的数据或资源。程序员通常使用HTTP请求来连接不同的应用程序，使它们能够共享数据并实现更多的功能。

## 如何发送HTTP请求
在Go语言中，我们可以使用内置的net/http包来发送HTTP请求。以下是一个简单的示例，使用GET请求从谷歌API获取搜索结果：
```
package main

import (
	"fmt"
	"net/http"
)

func main() {
	request, err := http.NewRequest("GET", "https://www.googleapis.com/customsearch/v1?key=123456&cx=017576662512468239146:omuauf_lfve&q=golang", nil)
	if err != nil {
		fmt.Println("请求错误: ", err)
	}
	response, err := http.DefaultClient.Do(request)
	if err != nil {
		fmt.Println("请求错误: ", err)
	}
	fmt.Println(response.Status)
}
```
输出结果为:
```
200 OK
```
请注意，您需要替换Google API密钥和自定义搜索ID以使示例代码生效。

## 深入了解
- 历史背景：HTTP协议最初由Tim Berners-Lee在1989年提出，用于在客户端和服务器之间传输超文本数据。
- 其他选项：除了Go语言的net/http包，还有其他流行的HTTP客户端库，如Postman和cURL。
- 实现细节：HTTP请求的实现包括构建并发送请求、等待服务器响应、处理响应数据等步骤。

## 查看更多信息
- [Go语言官方文档](https://golang.org/pkg/net/http/)
- [HTTP协议的历史](https://www.w3.org/Protocols/)
- [cURL文档](https://curl.haxx.se/docs/)