---
date: 2024-01-20 18:00:34.208592-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C\uFF1A \u4F7F\u7528Python\u7684`requests`\u5E93\
  \uFF0C\u53D1\u9001\u4E00\u4E2A\u7B80\u5355\u7684HTTP GET\u8BF7\u6C42\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.251523-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A\n\u4F7F\u7528Python\u7684`requests`\u5E93\
  \uFF0C\u53D1\u9001\u4E00\u4E2A\u7B80\u5355\u7684HTTP GET\u8BF7\u6C42\uFF1A."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

## How to:
如何操作：
使用Python的`requests`库，发送一个简单的HTTP GET请求：

```python
import requests

response = requests.get('https://api.github.com')
print(response.status_code)
print(response.json())
```

样例输出：

```
200
{'current_user_url': 'https://api.github.com/user', 'current_user_authorizations_html_url':...}
```

发送POST请求，并传递数据：

```python
import requests

data = {'key': 'value'}
response = requests.post('https://httpbin.org/post', data=data)
print(response.status_code)
print(response.json())
```

样例输出：

```
200
{
  ...
  'form': {
    'key': 'value'
  },
  ...
}
```

## Deep Dive
深入探讨：
HTTP请求的历史可以追溯到1990年代初的网络基础设施。Python的`urllib`库是处理HTTP的早期工具，但它的使用比较复杂。后来，`requests`库出现了，因其简洁且易于使用的接口受到喜爱。

替代方案包括使用`http.client`（更底层）、`aiohttp`（异步请求）以及各种HTTP客户端框架，如`Tornado`。底层实现涉及套接字编程和HTTP协议的遵从。

## See Also
查看更多：
- 官方requests库文档：https://requests.readthedocs.io/
- Python官方HTTP客户端库文档：https://docs.python.org/3/library/http.client.html
- HTTP协议详细信息：https://www.ietf.org/rfc/rfc2616.txt
- aiohttp文档：https://docs.aiohttp.org/
