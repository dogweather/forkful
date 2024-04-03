---
date: 2024-01-20 18:02:21.559635-07:00
description: "\u5982\u4F55\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.254884-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
