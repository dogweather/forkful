---
title:                "发出 HTTP 请求"
aliases:
- /zh/python/sending-an-http-request/
date:                  2024-01-20T18:00:34.208592-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
什么&为什么？
发送HTTP请求就是让你的程序向网路上的服务器请求数据或触发动作。程序员这样做可以与网络服务进行交互，获取资源或提交信息。

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
