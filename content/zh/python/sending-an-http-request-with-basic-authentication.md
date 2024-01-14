---
title:                "Python: 用基本身份验证发送http请求"
simple_title:         "用基本身份验证发送http请求"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

为什么：为什么有人会使用基本身份验证发送HTTP请求。

## Why (为什么)

通常，在发送HTTP请求时，服务器会要求用户提供身份验证信息以验证其身份。通过使用基本身份验证，用户可以以编程方式将其凭据发送给服务器，从而允许他们访问需要身份验证的资源。

## How To (如何做)

下面是使用基本身份验证发送HTTP请求的示例：

```Python
import requests

# 设置凭证
username = "用户名"
password = "密码"

# 构建认证头部
auth_header = requests.auth.HTTPBasicAuth(username, password)

# 发送GET请求到URL
response = requests.get("http://example.com/protected-resource", auth=auth_header)

# 打印响应
print(response.text)
```

输出：

```
这里是受保护的资源的内容。
```

## Deep Dive (深入探讨)

在上面的示例中，我们使用了Python的requests库来发送带有基本身份验证的HTTP请求。requests库提供了一个方便的方法来构建认证头部，只需提供用户名和密码即可。此外，我们还可以使用requests库发送其他类型的HTTP请求，如POST或DELETE。

基本身份验证使用Base64编码来发送凭据，因此它不是最安全的身份验证方法。建议在需要更高级别的安全性时，使用其他类型的身份验证，如OAuth或JWT。

## See Also (参考资料)

- [Python requests文档](https://docs.python-requests.org/en/latest/)
- [了解基本身份验证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [使用requests库发送HTTP请求的其他方法](https://realpython.com/python-requests/#more-complicated-post-requests)