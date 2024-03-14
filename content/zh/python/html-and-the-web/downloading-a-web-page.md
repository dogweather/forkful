---
date: 2024-01-20 17:44:50.435699-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u901A\u8FC7\u7F51\u7EDC\u6293\u53D6\
  \u7F51\u9875\u5185\u5BB9\u5230\u672C\u5730\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u6709\u8BF8\u591A\u539F\u56E0\uFF1A\u81EA\u52A8\u5316\u4FE1\u606F\u6536\u96C6\u3001\
  \u6570\u636E\u6316\u6398\u6216\u4E3A\u4E86\u5E94\u7528\u5F00\u53D1\u7B49\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.253836-06:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u901A\u8FC7\u7F51\u7EDC\u6293\u53D6\
  \u7F51\u9875\u5185\u5BB9\u5230\u672C\u5730\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u6709\u8BF8\u591A\u539F\u56E0\uFF1A\u81EA\u52A8\u5316\u4FE1\u606F\u6536\u96C6\u3001\
  \u6570\u636E\u6316\u6398\u6216\u4E3A\u4E86\u5E94\u7528\u5F00\u53D1\u7B49\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
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
