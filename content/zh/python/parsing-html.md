---
title:                "解析HTML"
date:                  2024-01-20T15:33:36.964458-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (什么是HTML解析以及为什么要进行HTML解析?)
HTML解析就是将网页上的HTML代码转换成可供程序使用的数据结构。程序员通常这样做是为了提取网页中的信息，比如链接、文本或者其他数据。

## How to: (如何进行HTML解析)
Python中进行HTML解析，我们通常使用`BeautifulSoup`库。下面的代码展示了如何用它来抓取网页标题：

```Python
from bs4 import BeautifulSoup
import requests

# 抓取网页
response = requests.get('http://example.com')
html = response.content

# 解析HTML
soup = BeautifulSoup(html, 'html.parser')

# 提取标题并打印
title = soup.find('title').get_text()
print(title)
```

运行这段代码，输出应该是网页的标题：

```
Example Domain
```

## Deep Dive (深入了解)
HTML解析可以追溯到万维网的早期。最初，提取数据不外乎遍历字符串。后来，出现了像`BeautifulSoup`、`lxml`和`html.parser`这样的库，它们的容错能力更强，使用也更为简便。

Python的标准库带有`html.parser`，但它速度比较慢且容错能力有限。`BeautifulSoup`是第三方库，易用而且功能丰富。它可以结合`html.parser`,`lxml`或`html5lib`使用。

除了抓取静态HTML外，有时你可能需要从动态内容生成的HTML中解析数据，这时可以使用`Selenium`或`Pyppeteer`等工具。

## See Also (延伸阅读)
- BeautifulSoup文档：https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Requests库文档：https://requests.readthedocs.io/en/master/
- `lxml`解析器：https://lxml.de/
- `html5lib`解析器：https://html5lib.readthedocs.io/en/latest/
- `Selenium`自动化工具：https://www.selenium.dev/documentation/en/
- `Pyppeteer`库：https://pyppeteer.github.io/pyppeteer/