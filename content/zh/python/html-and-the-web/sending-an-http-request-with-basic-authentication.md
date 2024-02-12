---
title:                "使用基本认证发送 HTTP 请求"
aliases: - /zh/python/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:21.559635-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在Python中发送带有基本认证的HTTP请求允许你安全地访问需要用户名和密码的网络资源。程序员这么做是因为一些网站的API要求使用基本认证来验证用户身份。

## 如何：
```python
import requests
from requests.auth import HTTPBasicAuth

# 替换成你的API端点
url = 'https://api.example.com/data'
# 替换成实际的用户名和密码
username = 'your_username'
password = 'your_password'

response = requests.get(url, auth=HTTPBasicAuth(username, password))

print(response.status_code)
print(response.text)
```

现在运行代码，你应该看到类似这样的输出：

```
200
{"example_key": "example_value"}
```

## 深入了解
HTTP基本认证是一种验证HTTP用户身份的方法，由于简单，它早在HTTP/1.0就被引入了。遗憾的是，基本认证不加密密码，相对不够安全，所以HTTPS越来越多地被用来加强安全性。

替代验证方式包括OAuth、API密钥或者像JWT（JSON Web Tokens）这样的更复杂的认证机制。

实现时，'requests'库做了大量工作。HTTPBasicAuth模块把用户名和密码转化为HTTP头信息（一个Base64编码的字符串），让服务器能够验证身份。

## 参见
- Python `requests`库文档：https://docs.python-requests.org/en/master/
- HTTP基本认证介绍：https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- 安全HTTP认证实践：https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html
