---
title:                "用基本身份验证发送http请求"
html_title:           "Python: 用基本身份验证发送http请求"
simple_title:         "用基本身份验证发送http请求"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

使用基本认证发送HTTP请求是指在发送HTTP请求时提供用户名和密码以确保安全性。程序员这样做的原因是为了在与服务器通信时保护敏感数据。

## 如何：

下面是使用Python发送带有基本认证的HTTP请求的示例代码：

```
import requests

url = 'https://example.com/api'
username = 'username'
password = 'password'

response = requests.get(url, auth=(username, password))
print(response.text)
```

输出如下：

```
{"message": "Success"}
```

## 深入探讨：

历史背景：基本认证是HTTP协议早期版本中的一种身份验证方式，在互联网发展初期使用较为广泛。

替代方案：除了基本认证，还有其他更安全的身份验证方式，如摘要认证和OAuth。

实现细节：使用Python的requests库可以方便地在HTTP请求中添加基本认证信息，提高程序的安全性和可靠性。

## 参考链接：

1. [Requests官方文档](https://docs.python-requests.org)
2. [HTTP 认证方法简介](https://www.ruanyifeng.com/blog/2011/11/http_authentication.html)
3. [网络安全：基本认证、摘要认证与SSL](https://www.cnblogs.com/vamei/archive/2012/06/03/2532182.html)