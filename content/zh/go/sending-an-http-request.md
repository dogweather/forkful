---
title:                "Go: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么
在现代的软件开发中，我们经常需要与服务器进行通信。发送HTTP请求是与服务器进行通信的一种常见方式，它可以让我们从服务器获取数据或者向服务器发送数据。无论是构建网站、移动应用程序，还是进行数据分析，发送HTTP请求都是必不可少的。因此，学习如何发送HTTP请求是非常重要的。

## 如何做
Go语言是一种快速、高效的编程语言，也是Google公司开发的。发送HTTP 请求在Go语言中非常简单。我们只需要使用内置的`http`包，调用`Get()`或者`Post()`函数即可。下面是一个简单的示例，展示如何使用Go语言发送一个GET请求，获取百度的主页:

```
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // 发送请求
    resp, err := http.Get("https://www.baidu.com/")

    // 检查是否有错误
    if err != nil {
        log.Fatal("http请求错误:", err)
    }

    // 读取响应的内容
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal("读取响应内容错误:", err)
    }

    // 将字节转换为字符串并打印出来
    fmt.Println(string(body))

    // 关闭响应的Body流
    resp.Body.Close()
}
```

运行上面的代码，我们将得到百度主页的HTML内容，如下所示：

```
<!doctype html>
<html>
<head>
<title>百度一下，你就知道</title>
...

```

在上面的代码中，我们首先引入了`fmt`、`io/ioutil`和`net/http`包。然后，我们使用`Get()`函数发送一个GET请求，获取百度主页的HTML内容。之后，我们读取响应的内容并将其转换为字符串并打印出来。最后，我们务必关闭响应的Body流，以避免资源泄露。

同样的，我们也可以使用`Post()`函数来发送POST请求：

```
// 导入包
import (
    "bytes"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // 定义POST请求的参数
    data := bytes.NewBuffer([]byte("key1=value1&key2=value2"))

    // 发送POST请求
    resp, err := http.Post("https://example.com/api", "application/x-www-form-urlencoded", data)
    if err != nil {
        log.Fatal("http请求错误:", err)
    }

    // 读取响应的内容
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal("读取响应内容错误:", err)
    }

    // 将字节转换为字符串并打印出来
    fmt.Println(string(body))

    // 关闭响应的Body流
    resp.Body.Close()
}
```

上面的示例中，我们首先定义了POST请求的参数`data`，并使用`Post()`函数发送请求。注意，在`Post()`函数中，我们指定了`Content-Type`为`application/x-www-form-urlencoded`，表示发送的数据为表单格式。最后，我们读取响应的内容并将其转换为字符串并打印出来。

## 深入了解
除了简单的`Get()`和`Post()`函数外，Go语言的`http`包还提供了许多其他可以帮助我们定制HTTP请求的功能。例如，我们可以设置请求的超时时间、自定义请求的Header、发送带有Cookie的请求等等。如果想要更深入地了解如何使用Go语言发送HTTP请求，可以参考官方文档：[https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)

## 请参考
[Go语言官方文档-发送HTTP请求](https://golang.org/pkg/net/http/)