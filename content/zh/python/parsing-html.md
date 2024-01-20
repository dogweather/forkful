---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/parsing-html.md"
---

{{< edit_this_page >}}

## 这是什么和为什么？ (What & Why?)

HTML解析，是指计算机程序读取HTML（超文本标记语言）文本，分析其结构，转化为可以操作的数据结构，如DOM树。程序员之所以要解析HTML，是因为这可以帮助他们抓取和分析网页数据，改善用户的网络体验。

## 怎么做？ (How to?)

我们使用Python中的`beautifulsoup4`库来完成HTML解析。首先，需要安装此库：

```
pip install beautifulsoup4
```

然后，我们来看一段示例代码：

```python
from bs4 import BeautifulSoup

html_doc = """
<html><head><title>The Dormouse's story</title></head>
<body>
<p class="title"><b>The Dormouse's story</b></p>
"""
soup = BeautifulSoup(html_doc, 'html.parser')
print(soup.prettify())
```

运行这段代码，输出是：

```
<html>
 <head>
  <title>
   The Dormouse's story
  </title>
 </head>
 <body>
  <p class="title">
   <b>
    The Dormouse's story
   </b>
  </p>
 </body>
</html>
```

## 深入研究 (Deep Dive)

HTML解析的历史可以追溯到90年代初，互联网刚开始流行的时候。那时，大部分网页都是静态HTML，因此，解析HTML成为提取网页内容的主要方法。

此外，不同于用Python编写的`beautifulsoup4`库，还有一些其它的解析器，例如Ruby的`Nokogiri`，Java的`Jsoup`等等。

在实现细节上，`beautifulsoup4`可以处理包括不良结构在内的各种HTML。它首先会将HTML进行词法分析，生成标记(token)，然后将这些标记组织成一棵解析树。

## 参考资料 (See Also)

- Python BeautifulSoup官网: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- W3Schools HTML教程: https://www.w3schools.com/html/
- Mozilla HTML教程: https://developer.mozilla.org/en-US/docs/Web/HTML