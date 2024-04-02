---
date: 2024-01-20 18:02:21.559635-07:00
description: "\u5728Python\u4E2D\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684\
  HTTP\u8BF7\u6C42\u5141\u8BB8\u4F60\u5B89\u5168\u5730\u8BBF\u95EE\u9700\u8981\u7528\
  \u6237\u540D\u548C\u5BC6\u7801\u7684\u7F51\u7EDC\u8D44\u6E90\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u4E00\u4E9B\u7F51\u7AD9\u7684API\u8981\u6C42\
  \u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u6765\u9A8C\u8BC1\u7528\u6237\u8EAB\u4EFD\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.254884-06:00'
model: gpt-4-1106-preview
summary: "\u5728Python\u4E2D\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684\
  HTTP\u8BF7\u6C42\u5141\u8BB8\u4F60\u5B89\u5168\u5730\u8BBF\u95EE\u9700\u8981\u7528\
  \u6237\u540D\u548C\u5BC6\u7801\u7684\u7F51\u7EDC\u8D44\u6E90\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u4E00\u4E9B\u7F51\u7AD9\u7684API\u8981\u6C42\
  \u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u6765\u9A8C\u8BC1\u7528\u6237\u8EAB\u4EFD\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
