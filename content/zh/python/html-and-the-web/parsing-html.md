---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:48.467993-07:00
description: "\u5982\u4F55\uFF1A Python\u63D0\u4F9B\u4E86\u50CFBeautifulSoup\u548C\
  requests\u8FD9\u6837\u7684\u5F3A\u5927\u5E93\u7528\u4E8E\u7F51\u7EDC\u6293\u53D6\
  \u548CHTML\u89E3\u6790\u3002\u9996\u5148\uFF0C\u5982\u679C\u4F60\u8FD8\u6CA1\u6709\
  \u5B89\u88C5\u8FD9\u4E9B\u5E93\uFF0C\u4F60\u9700\u8981\u5B89\u88C5\u5B83\u4EEC\uFF1A\
  ."
lastmod: '2024-03-13T22:44:47.252774-06:00'
model: gpt-4-0125-preview
summary: "Python\u63D0\u4F9B\u4E86\u50CFBeautifulSoup\u548Crequests\u8FD9\u6837\u7684\
  \u5F3A\u5927\u5E93\u7528\u4E8E\u7F51\u7EDC\u6293\u53D6\u548CHTML\u89E3\u6790\u3002\
  \u9996\u5148\uFF0C\u5982\u679C\u4F60\u8FD8\u6CA1\u6709\u5B89\u88C5\u8FD9\u4E9B\u5E93\
  \uFF0C\u4F60\u9700\u8981\u5B89\u88C5\u5B83\u4EEC\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何：
Python提供了像BeautifulSoup和requests这样的强大库用于网络抓取和HTML解析。首先，如果你还没有安装这些库，你需要安装它们：

```bash
pip install beautifulsoup4 requests
```

这里有一个使用`requests`来获取网页的HTML内容和使用`BeautifulSoup`来解析它的基本示例：

```python
import requests
from bs4 import BeautifulSoup

# 获取网页的内容
URL = 'https://example.com'
page = requests.get(URL)

# 解析HTML内容
soup = BeautifulSoup(page.content, 'html.parser')

# 提取网页标题的示例
title = soup.find('title').text
print(f'网页标题: {title}')
```

**示例输出**:
```
网页标题: 示例域名
```

对于更复杂的查询，例如提取网页上的所有链接，你可以使用BeautifulSoup的各种方法来导航和搜索解析树：

```python
# 提取<a>标签内的所有链接
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**示例输出**:
```
https://www.iana.org/domains/example
```

BeautifulSoup的灵活性允许你根据需要调整搜索，以准确获取所需的数据，使HTML解析成为与Web内容打交道的程序员的强大工具。
