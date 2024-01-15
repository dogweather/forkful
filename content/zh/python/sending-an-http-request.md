---
title:                "发送一个http请求"
html_title:           "Python: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求在Python编程中非常重要，因为它可以让我们与网络服务进行交互。例如，我们可以使用HTTP请求来获取网页的内容或向网络服务器发送数据。

## 如何做

```Python
import requests

# 发送GET请求
response = requests.get("https://www.google.com")
print(response.status_code) # 输出200
print(response.text) # 输出网页的内容

# 发送POST请求
data = {'username': 'John', 'password': '1234'}
response = requests.post("https://www.example.com/login", data=data)
print(response.status_code) # 输出200
print(response.json()) # 输出服务器返回的JSON数据
```

## 深入探讨

发送HTTP请求的核心方法是使用`requests`库中的`get()`和`post()`方法。这两个方法可以传入URL和一些可选的参数来发送不同类型的HTTP请求。除此之外，我们还可以通过`response`对象来获取服务器返回的信息，如状态码和内容。

## 参考资料

- [Python官方文档](https://docs.python.org/3/library/urllib.html)
- [Requests库官方文档](https://requests.readthedocs.io/en/master/)
- [HTTP请求教程](https://www.runoob.com/http/http-tutorial.html)