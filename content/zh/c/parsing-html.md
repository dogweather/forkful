---
title:                "解析HTML"
date:                  2024-01-20T15:30:23.458772-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

category:             "C"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

HTML解析就是将HTML文档转换成对计算机程序友好的数据结构。程序员这么做是为了操作和获取网页内容，实现网页数据抓取、自动化测试等功能。

## How to (如何做)

在C语言中，可以使用libxml2库来解析HTML。以下展示基本用法：

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc;
    htmlNodePtr cur;

    // 初始化库
    htmlInitParser();
    
    // 解析HTML字符串
    const char *html = "<html><body><p>Hello, World!</p></body></html>";
    doc = htmlReadDoc((xmlChar*)html, NULL, NULL, HTML_PARSE_NOERROR | HTML_PARSE_RECOVER);

    // 获取根节点
    cur = xmlDocGetRootElement(doc);

    // 遍历文档
    cur = cur->xmlChildrenNode;
    while (cur != NULL) {
        if (cur->type == XML_ELEMENT_NODE) {
            printf("Node type: Element, name: %s\n", cur->name);
        }
        cur = cur->next;
    }

    // 释放文档
    xmlFreeDoc(doc);
    
    // 清理库
    xmlCleanupParser();

    return 0;
}
```

Sample output:

```
Node type: Element, name: body
Node type: Element, name: p
```

## Deep Dive (深入了解)

历史背景：HTML解析有很多工具和库，早期的解析器通常依赖严格的HTML规则，但现代解析器，如libxml2，能处理不完整或错误的HTML，使其更加健壮。

备选方案：除了libxml2，还有Gumbo、MyHTML等库。

实现细节：HTML解析库通常使用DOM（文档对象模型）来表示HTML文档结构，它将页面转换为节点树，便于查询和操作。

## See Also (另请参阅)

- [libxml2官方文档](http://xmlsoft.org/html/libxml-HTMLparser.html)
- [Gumbo解析器](https://github.com/google/gumbo-parser)
- [MyHTML库](https://github.com/lexborisov/myhtml)
