---
date: 2024-01-20 18:02:21.559635-07:00
description: "\u5982\u4F55\uFF1A HTTP\u57FA\u672C\u8BA4\u8BC1\u662F\u4E00\u79CD\u9A8C\
  \u8BC1HTTP\u7528\u6237\u8EAB\u4EFD\u7684\u65B9\u6CD5\uFF0C\u7531\u4E8E\u7B80\u5355\
  \uFF0C\u5B83\u65E9\u5728HTTP/1.0\u5C31\u88AB\u5F15\u5165\u4E86\u3002\u9057\u61BE\
  \u7684\u662F\uFF0C\u57FA\u672C\u8BA4\u8BC1\u4E0D\u52A0\u5BC6\u5BC6\u7801\uFF0C\u76F8\
  \u5BF9\u4E0D\u591F\u5B89\u5168\uFF0C\u6240\u4EE5HTTPS\u8D8A\u6765\u8D8A\u591A\u5730\
  \u88AB\u7528\u6765\u52A0\u5F3A\u5B89\u5168\u6027\u3002 \u66FF\u4EE3\u9A8C\u8BC1\u65B9\
  \u5F0F\u5305\u62ECOAuth\u3001API\u5BC6\u94A5\u6216\u8005\u50CFJWT\uFF08JSON Web\
  \ Tokens\uFF09\u8FD9\u6837\u7684\u66F4\u590D\u6742\u7684\u8BA4\u8BC1\u673A\u5236\
  \u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.482561-06:00'
model: gpt-4-1106-preview
summary: "\u66FF\u4EE3\u9A8C\u8BC1\u65B9\u5F0F\u5305\u62ECOAuth\u3001API\u5BC6\u94A5\
  \u6216\u8005\u50CFJWT\uFF08JSON Web Tokens\uFF09\u8FD9\u6837\u7684\u66F4\u590D\u6742\
  \u7684\u8BA4\u8BC1\u673A\u5236\u3002"
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
