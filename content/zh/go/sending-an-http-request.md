---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么和为什么？
HTTP请求是一个让程序与互联网上的其他服务器进行数据交互的方式。程序员之所以发送HTTP请求，是因为这是他们获取、制作、更新或删除网络上数据的主要手段。

## 如何操作
以下是一段简单的在Go中发送HTTP GET请求的代码：

```Go
package main

import (
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	response, err := http.Get("http://webcode.me")
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	data, err := ioutil.ReadAll(response.Body)
	if err != nil {
		log.Fatal(err)
	}

	log.Println(string(data))
}
```
运行上述程序，你将看到从`http://webcode.me`获取的HTML内容输出。

## 深入探讨
在HTTP兴起之初，大多数程序员需要手动在TCP层建立连接，这是一个颇为复杂且易出错的过程。随着HTTP请求的普及，如今我们可以便捷安全地获取或发送数据。

除了使用核心`net/http`包外，你也可以选择第三方的HTTP请求库，例如`gorilla/mux`，这些库可能提供了一些额外的功能和优化。

就实现细节来说，HTTP请求包括将请求信息装入特定的数据格式，通过TCP/IP协议将其发送到服务器，最后接收并解码服务器的回应。

## 另请参阅
- Go官方文档对`net/http`包的说明：https://golang.org/pkg/net/http/
- 第三方HTTP请求库‘gorilla/mux’的Github页面：https://github.com/gorilla/mux
- 更深入的了解HTTP请求的文章：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview