---
date: 2024-01-20 18:00:34.208592-07:00
description: "\u4EC0\u4E48&\u4E3A\u4EC0\u4E48\uFF1F \u53D1\u9001HTTP\u8BF7\u6C42\u5C31\
  \u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\u5411\u7F51\u8DEF\u4E0A\u7684\u670D\u52A1\u5668\
  \u8BF7\u6C42\u6570\u636E\u6216\u89E6\u53D1\u52A8\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u53EF\u4EE5\u4E0E\u7F51\u7EDC\u670D\u52A1\u8FDB\u884C\u4EA4\u4E92\uFF0C\
  \u83B7\u53D6\u8D44\u6E90\u6216\u63D0\u4EA4\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.251523-06:00'
model: gpt-4-1106-preview
summary: "\u4EC0\u4E48&\u4E3A\u4EC0\u4E48\uFF1F\n\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\
  \u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\u5411\u7F51\u8DEF\u4E0A\u7684\u670D\u52A1\u5668\
  \u8BF7\u6C42\u6570\u636E\u6216\u89E6\u53D1\u52A8\u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u53EF\u4EE5\u4E0E\u7F51\u7EDC\u670D\u52A1\u8FDB\u884C\u4EA4\u4E92\uFF0C\
  \u83B7\u53D6\u8D44\u6E90\u6216\u63D0\u4EA4\u4FE1\u606F\u3002."
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
