---
title:                "下载网页"
html_title:           "Python: 下载网页"
simple_title:         "下载网页"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

要下载一个网页可以带来许多好处。首先，您可以在离线的情况下阅读网页内容，而不必担心网络连接问题。其次，您可以保存您喜欢的网页，以便以后再次阅读或分享给他人。

## 如何

下面是使用Python下载网页的简单代码示例和输出。

```Python
# 导入必要的模块
import requests

# 定义要下载的网址
url = "https://www.example.com"

# 发送GET请求，并将响应存储在变量中
response = requests.get(url)

# 打印响应内容
print(response.text)
```

这将输出网页的HTML内容。您也可以使用Python中的“urllib”模块来下载网页。代码示例如下：

```Python
# 导入必要的模块
import urllib.request

# 定义要下载的网址
url = "https://www.example.com"

# 发送GET请求，并将响应存储在变量中
response = urllib.request.urlopen(url)

# 读取并打印响应内容
data = response.read()
print(data)
```

## 深入探讨

使用Python下载网页的另一个重要方面是处理可能出现的错误。例如，如果无法连接到网站，您将收到一个错误。为了解决这个问题，您可以使用“try-except”语句来捕获异常并处理它们。代码示例如下：

```Python
# 导入必要的模块
import requests

# 定义要下载的网址
url = "https://www.example.com"

# 发送GET请求，并将响应存储在变量中
try:
    response = requests.get(url)
    # 打印响应内容
    print(response.text)
except Exception as e:
    # 处理异常情况
    print(e)
```

您可以根据自己的需要使用不同的异常类型来处理不同的错误。

## 参考链接

- [Python requests模块文档](https://requests.readthedocs.io/en/master/)
- [Python urllib模块文档](https://docs.python.org/3/library/urllib.html)
- [Python异常处理文档](https://docs.python.org/3/tutorial/errors.html)