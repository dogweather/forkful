---
date: 2024-01-26 04:35:10.040963-07:00
description: "\u201C\u5904\u7406XML\u201D\u6307\u7684\u662F\u4F7F\u7528\u7F16\u7A0B\
  \u8BFB\u53D6\u3001\u521B\u5EFA\u548C\u4FEE\u6539XML\uFF08\u53EF\u6269\u5C55\u6807\
  \u8BB0\u8BED\u8A00\uFF09\u6587\u4EF6\u7684\u8FC7\u7A0B\u3002\u7F16\u7A0B\u8005\u4E4B\
  \u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u56E0\u4E3AXML\u56E0\u5176\u5E73\u53F0\
  \u65E0\u5173\u6027\u548C\u81EA\u63CF\u8FF0\u683C\u5F0F\u800C\u5E7F\u6CDB\u7528\u4E8E\
  \u6570\u636E\u4EA4\u6362\u3002"
lastmod: '2024-03-13T22:44:47.283688-06:00'
model: gpt-4-0125-preview
summary: "\u201C\u5904\u7406XML\u201D\u6307\u7684\u662F\u4F7F\u7528\u7F16\u7A0B\u8BFB\
  \u53D6\u3001\u521B\u5EFA\u548C\u4FEE\u6539XML\uFF08\u53EF\u6269\u5C55\u6807\u8BB0\
  \u8BED\u8A00\uFF09\u6587\u4EF6\u7684\u8FC7\u7A0B\u3002\u7F16\u7A0B\u8005\u4E4B\u6240\
  \u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u56E0\u4E3AXML\u56E0\u5176\u5E73\u53F0\u65E0\
  \u5173\u6027\u548C\u81EA\u63CF\u8FF0\u683C\u5F0F\u800C\u5E7F\u6CDB\u7528\u4E8E\u6570\
  \u636E\u4EA4\u6362\u3002."
title: "\u5904\u7406XML"
weight: 40
---

## 如何做：
Python的`xml.etree.ElementTree`模块提供了处理XML的工具。

解析XML文档：
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'标题: {title}, 作者: {author}')
```
示例输出：
```
标题: Learning Python, 作者: Mark Lutz
标题: Programming Python, 作者: Mark Lutz
```

创建XML文档：
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## 深入了解：
XML自90年代末以来一直存在，作为SGML的简化子集被创建，以便于在线数据共享。尽管JSON对于web数据的流行日增，XML在许多企业、配置和web服务（SOAP、RSS）中仍然至关重要。

`xml.etree.ElementTree`的替代品包括`lxml`和`minidom`。`lxml`速度更快，功能更丰富，而`minidom`提供了更“像DOM”的XML接口。在选择时，考虑易用性、性能和特定的功能需求。

在底层，`ElementTree`操作的是一个元素树模型，其中XML文件的每个组件都是树中的一个节点。这允许了直接的路径表达和搜索，使得导航和操作XML数据的结构变得更加简单。

## 另请参阅：
- Python `xml.etree.ElementTree`模块：https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`：https://lxml.de/
- W3Schools XML教程：https://www.w3schools.com/xml/
- XML规范：https://www.w3.org/XML/
