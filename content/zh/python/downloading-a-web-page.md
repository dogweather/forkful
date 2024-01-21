---
title:                "下载网页"
date:                  2024-01-20T17:44:50.435699-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)
下载网页就是通过网络抓取网页内容到本地。程序员这么做有诸多原因：自动化信息收集、数据挖掘或为了应用开发等。

## How to: (如何操作)
使用Python的`requests`库能轻松完成网页下载。下面是个示例：

```Python
import requests

url = 'https://www.example.com'
response = requests.get(url)

# 确认请求成功
if response.status_code == 200:
    content = response.text
    print(content[:100])  # 打印前100个字符作为示例
else:
    print('Failed to retrieve the web page.')

```

运行以上代码，你可能会得到类似这样的输出：

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
```

## Deep Dive (深入探究)
早期，网页下载经常使用`urllib`。现在更多使用`requests`，它界面友好，易用性强。除了`requests`，`aiohttp`为异步操作提供支持。要处理动态内容则需要`Selenium`或`Pyppeteer`。

下载网页时需要处理各种细节：如编码问题、网络错误、重定向处理等。`requests`库隐藏了这些复杂性。

网速慢或网页大时，应该考虑流式下载。`requests`可以这么做：

```Python
response = requests.get(url, stream=True)
```

此外，还需考虑慎用爬虫，遵守`robots.txt`规则。

## See Also (查看更多)
- `requests`库文档: https://docs.python-requests.org/en/latest/
- Python官方`urllib`文档: https://docs.python.org/3/library/urllib.html
- `Selenium`文档: https://selenium-python.readthedocs.io/
- `Pyppeteer` GitHub页面: https://github.com/pyppeteer/pyppeteer
- 关于`robots.txt`的信息: https://www.robotstxt.org/