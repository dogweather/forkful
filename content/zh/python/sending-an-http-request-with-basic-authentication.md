---
title:                "发送HTTP请求与基本身份验证"
html_title:           "Python: 发送HTTP请求与基本身份验证"
simple_title:         "发送HTTP请求与基本身份验证"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求是Web开发中最基本的步骤之一。使用基本认证可以对请求进行安全保护，确保只有授权的用户可以访问相关的数据或资源。

## 如何

```Python
import requests

url = "https://example.com/api"
username = "user123"
password = "password123"

# 创建Basic Auth认证对象
auth = requests.auth.HTTPBasicAuth(username, password)

# 发送HTTP请求并传入auth参数
response = requests.get(url, auth=auth)

# 打印响应内容
print(response.text)

# 输出：请求成功返回数据
```

## 深入讨论

发送HTTP请求时，通过添加特定的header或参数可以实现不同类型的认证。基本认证是最简单也是最常用的一种认证方式。它需要在每次请求时，在HTTP头部中添加一个包含用户名和密码的Authorization字段，用于对服务器进行身份验证。同时，建议在使用基本认证时，通过HTTPS协议进行数据传输，以保证安全性。

## 参考链接

- [Python requests库官方文档](https://2.python-requests.org//zh_CN/latest/user/authentication.html)
- [Web开发常用的请求认证方式介绍](https://www.cnblogs.com/cff210167/p/5618659.html)
- [使用Python发送基本认证请求的示例代码](https://www.geeksforgeeks.org/basic-authentication-in-python/)