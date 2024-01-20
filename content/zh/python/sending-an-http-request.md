---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么和为什么?

HTTP请求是您的程序向网络服务器发送内容或请求内容的手段。程序员经常要发HTTP请求来使用Web API，接收数据，或者进行网络通信。

## 如何实施:

在Python中，一个常用的库来发送HTTP请求是requests。以下是一个GET请求的简单示例：

```Python
import requests

response = requests.get('http://www.baidu.com')
print(response.status_code)
print(response.text)
```

运行上述代码，您应该看到以下输出：

```bash
200
<!DOCTYPE html>
<!--STATUS OK-->
...（此处省略了百度首页的HTML代码）
```

## 深度剖析

请求库背后的历史背景是，之前的库 urllib 和 httplib 都不够友好和方便，所以Kenneth Reitz制造了requests库，在Python社区获得了广泛的欢迎和应用。

当然，除了 requests 库外，Python 中还有其他一些库也可以发送HTTP请求，比如 httplib2、treq 等，你可以根据自己的需求来选择。

在内部实现方面, 使用request库是向服务器发送请求的步骤: 首先, 我们使用`requests.get()`方法发出请求, 然后服务器会返回一个Response对象。这个对象包括了所有关于这个HTTP响应的信息。

## 另请参阅

- [Requests:HTTP for Humans](https://requests.readthedocs.io/en/master/)
- [Python官方文档](https://docs.python.org/3/library/http.client.html)