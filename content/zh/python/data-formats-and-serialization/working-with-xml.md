---
title:                "处理XML"
aliases:
- /zh/python/working-with-xml/
date:                  2024-01-26T04:35:10.040963-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么和为什么？
“处理XML”指的是使用编程读取、创建和修改XML（可扩展标记语言）文件的过程。编程者之所以这样做，是因为XML因其平台无关性和自描述格式而广泛用于数据交换。

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
