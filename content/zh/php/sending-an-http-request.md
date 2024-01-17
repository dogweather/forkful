---
title:                "发送HTTP请求"
html_title:           "PHP: 发送HTTP请求"
simple_title:         "发送HTTP请求"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 什么是HTTP请求？为什么程序员要发送它？

发送HTTP请求是发送数据和获取数据的一种通用方式。程序员通常会发送HTTP请求来与网络上的其他服务器进行交互，例如从远程服务器获取数据或者将数据发送到远程服务器。

# 如何发送HTTP请求？

使用PHP内置的函数`file_get_contents()`可以很容易地发送HTTP请求。您只需要提供远程URL地址作为参数，并将返回的响应存储在变量中即可。比如，如果您想要获取来自Google网站的HTML内容，您可以像这样进行操作：

```
$google = file_get_contents('https://www.google.com/');
echo $google; // 输出来自Google网站的HTML内容
```

# 深入了解

HTTP请求是HTTP协议的一部分，它是一种规定了客户端和服务器之间通信的标准化方式。在Web开发中，程序员经常使用HTTP请求来获取数据、处理表单提交、上传文件等。除了使用`file_get_contents()`这种方式，程序员还可以使用cURL和其他HTTP客户端库来发送HTTP请求。

# 相关资源

- [PHP官方文档- HTTP请求](https://www.php.net/manual/en/book.http.php)
- [W3School- PHP发送HTTP请求](https://www.w3schools.com/php/func_http_file_get_contents.asp)