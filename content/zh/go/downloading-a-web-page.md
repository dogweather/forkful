---
title:                "下载网页"
html_title:           "Go: 下载网页"
simple_title:         "下载网页"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为何
你有没有遇到过想要保存网页内容的情况？那么，有了 Go 语言你就可以轻松地做到！

## 如何
下载网页在 Go 中非常简单，只需要用到内置的 `net/http` 和 `io/ioutil` 库即可。首先，我们需要定义一个函数来处理下载任务，并指定一个网页的 URL。

```Go
func downloadPage(url string) {
    // 发送 HTTP 请求，获取网页的响应
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    defer resp.Body.Close()

    // 将响应的内容读取出来
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // 打印网页内容
    fmt.Println(string(body))
}

// 调用函数，传入需要下载的网页 URL
downloadPage("https://www.example.com")
```

运行上面的代码，你就可以在终端中看到网页的内容了。

## 深入了解
除了直接通过 URL 下载网页，Go 还可以通过 `http.Client` 来设置一些额外的参数，例如设置请求头、超时时间等。同时，还可以将下载的网页保存为文件，而不仅仅是打印出来。

## 参考链接
- [Go 官方文档 - net/http 包](https://golang.org/pkg/net/http/)
- [Go 官方文档 - io/ioutil 包](https://golang.org/pkg/io/ioutil/)
- [Go 配套中文文档 - 标准库](https://go-zh.org/pkg/)