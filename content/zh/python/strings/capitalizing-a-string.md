---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Python \u6709\u4E00\u4E2A\u5185\u5EFA\
  \u7684\u5B57\u7B26\u4E32\u65B9\u6CD5 `.capitalize()` \uFF0C\u53EF\u4EE5\u8F7B\u677E\
  \u5B8C\u6210\u8FD9\u4E2A\u4EFB\u52A1\u3002"
lastmod: '2024-04-05T21:53:47.593041-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u5316\u4E3A\u5927\u5199"
weight: 2
---

## 如何操作：

### 使用 Python 的内建方法：
Python 有一个内建的字符串方法 `.capitalize()` ，可以轻松完成这个任务。

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**输出：**
```
Hello world
```

以下是我为构建此站点而使用的定制版 `capitalize()`。我需要确保像 **HTML** 这样的特殊单词始终保持全部大写。这也展示了 [doctests](https://docs.python.org/3/library/doctest.html) 的使用：

```python
def capitalize(string: str) -> str:
    """
    将字符串首字母大写。
    处理像“HTML”这样的特殊情况。

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```

### 处理多个单词：
对于希望字符串中的每个单词都以大写字母开头的场景（如标题），可以使用 `.title()` 方法。

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**输出：**
```
Python Programming Essentials
```

### 使用第三方库：
尽管 Python 的标准库适用于基本的字符串首字母大写操作，但像 `textblob` 这样的库可以提供更细致的控制，特别是对于自然语言处理。

首先，确保您已安装 `textblob`：
```bash
pip install textblob
```

然后，使用它来大写字符串，记住 `textblob` 的大写方法可能会根据使用上下文有所不同：

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**输出：**
```
This is a test sentence
```

记住，尽管 `capitalize()` 和 `title()` 方法普遍有用，但利用像 `textblob` 这样的库可以为特定应用提供额外的灵活性。
