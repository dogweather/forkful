---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么和为什么？
发送带Basic认证的HTTP请求是指用用户名和密码进行身份验证然后发送HTTP请求。程序员通过它来确保数据安全性，防止未经授权的访问。

## 如何：
在Python中，我们可以使用内置库`requests`进行HTTP请求和身份验证。以下是代码示例和输出：

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://api.github.com/user', auth=HTTPBasicAuth('user', 'pass'))
print(response.status_code)
```

程序输出将是`200`，意味着请求已成功接收并理解。

## 深入探讨
**历史背景**：HTTP Basic 认证是一种用于HTTP协议的简单身份认证技术。这种技术最早于1999年在RFC 2617文档中描述。

**替代方案**：虽然Basic认证简单易用，但并不一定安全，因为它将用户名和密码编码为Base64，易被破解。现在更安全的替代方案是使用OAuth，JWT或Bearer tokens进行身份验证。

**实现细节**：在发送带有Basic身份验证的HTTP请求时，客户端将用户名和密码组合成一个字符串，然后用Base64对其进行编码，并将它放入HTTP header的`Authorization`字段。

## 另请参阅
1. Python `requests`库文档：http://docs.python-requests.org/
2. Python 所有 HTTP 请求类型： http://tools.ietf.org/html/rfc2616
3. HTTP Basic 认证详情：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Headers/Authorization