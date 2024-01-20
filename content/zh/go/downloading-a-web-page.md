---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

网页下载是获取网站 HTML 源代码的过程，通常会用于数据处理或内容生成. 程序员这么做是为了利用该网页的信息，或者备份网页内容等等。

## 如何做：

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	response, err := http.Get("https://example.com")
	if err != nil {
		panic(err)
	}
	defer response.Body.Close()

	bytes, err := ioutil.ReadAll(response.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println("HTML:\n\n", string(bytes))
}
```

运行上面的代码，你会看到： ```HTML:\n\n``` 后面跟着你请求网页的用户可读形式的 HTML。

## 深入研究

网页下载基于 HTTP 或 HTTPS 协议，并且已经使用多年。在下载之前，你可以通过一些特定的 HTTP 头部来定制你的请求，例如 "User-Agent"，"Accept-Language" 等。不同的网络库或工具，如 cURL，Wget，或 Python 的 requests 库提供了下载网页的功能。然而，直接在 Go 语言中使用 `net/http` 包是很直接的。

## 更多参考

我推荐以下链接给你，以便你可以更深入地理解和掌握这个话题：

Go 网络编程教程：https://books.studygolang.com/gopl-zh/
Go 语言 net/http 包文件手册：https://pkg.go.dev/net/http
处理 HTTP 请求的 Go 语言实践：https://www.alexedwards.net/blog/a-recap-of-request-handling