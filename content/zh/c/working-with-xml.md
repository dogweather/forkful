---
title:                "处理XML"
date:                  2024-01-26T04:28:12.756084-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 C 中处理 XML 涉及解析、创建和操作 XML 文件 - 本质上是结构化数据存储。程序员这样做是为了与以便携式和人类可读的格式交互数据，这种格式通常用于配置、数据交换等。

## 如何操作：
以下是使用 `libxml2` 库解析 XML 文件并获取根元素的代码片段。

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // 解析 XML 文件
    doc = xmlReadFile("example.xml", NULL, 0);

    // 获取根元素
    root_element = xmlDocGetRootElement(doc);

    printf("Root Element: %s\n", root_element->name);

    // 释放文档
    xmlFreeDoc(doc);

    // 清理解析器
    xmlCleanupParser();

    return 0;
}
```

对于根为 `<data>` 的 XML，示例输出可能是：
```
Root Element: data
```

## 深入了解
XML，或可扩展标记语言，可以追溯到 90 年代末期，提供了一种描述和结构化数据的方法。在 C 中，`libxml2` 是首选。它功能强大，尽管对 XML 新手来说并不是最容易的。替代方案包括 `tinyxml2`，它更轻巧、更适合初学者。就实现而言，C 没有内置的 XML 支持，所以库填补了这个空白。它们在大小、速度、复杂性和可移植性方面各不相同。大多数提供 DOM 和 SAX 解析方法：DOM 将整个文件加载到内存中，适用于小型文档；SAX 是事件驱动的，在运行时处理元素，更适合大型文件。两者都有它们的用例和权衡。

## 另请参阅
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 on GitHub](https://github.com/leethomason/tinyxml2)
- [w3schools 上的 XML 教程](https://www.w3schools.com/xml/)
- [W3C 的 XML 规范](https://www.w3.org/XML/)
