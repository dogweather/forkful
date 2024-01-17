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

## 什么和为什么？

下载网页是指从互联网上将网页内容保存到本地计算机的过程。程序员通常会下载网页来进行数据抓取、分析和处理，以及构建网站和应用程序。

## 如何：

```Python
# 导入必要的模块
import requests

# 使用requests发送GET请求，下载网页并保存为HTML文件
url = "https://www.example.com"
response = requests.get(url)
with open("example.html", "w") as f:
    f.write(response.text)

# 打印出网页的内容
print(response.text)

# 使用BeautifulSoup库来解析和提取网页中的数据
from bs4 import BeautifulSoup
soup = BeautifulSoup(response.text, "html.parser")
# 找到所有的链接
links = soup.find_all("a")
# 遍历链接并打印出链接的文本和URL
for link in links:
    print(link.text)
    print(link.get("href"))
```

输出：
```html
<!DOCTYPE html>
<html>
<head>
    <title>Example Website</title>
</head>
<body>
    <h1>Hello World</h1>
    <p>This is an example website.</p>
    <a href="https://www.google.com">Google</a>
    <a href="https://www.yahoo.com">Yahoo</a>
    <a href="https://www.bing.com">Bing</a>
</body>
</html>

Google
https://www.google.com
Yahoo
https://www.yahoo.com
Bing
https://www.bing.com
```

## 深入探讨：

历史背景：在互联网发展的早期，下载网页是非常常见的，因为那时常用的阅览器并不支持离线浏览网页。现在，下载网页的主要目的是为了进行数据分析和构建网站和应用程序。

其他方法：除了使用Python代码来下载网页，也可以使用命令行工具，如wget和curl。这些工具能够更快速和有效地下载网页，并且能够处理一些特殊情况，如连接超时和重定向。

实现细节：Python中最常用的库之一是requests，它提供了一个简单和直观的接口来发送HTTP请求并处理响应。要解析网页中的数据，可以使用BeautifulSoup库，它能够根据HTML标签来提取数据，并且还能够处理不规则的HTML代码。

## 查看也许感兴趣的：

- [Requests 官方文档](https://docs.python-requests.org/en/master/)
- [BeautifulSoup 官方文档](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [wget 命令行工具](https://www.gnu.org/software/wget/)
- [curl 命令行工具](https://curl.se/)