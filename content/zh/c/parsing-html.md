---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-html.md"
---

{{< edit_this_page >}}

## 什么和为什么？

解析HTML是将HTML文本（属于‘标记语言’）转换成其组件和层级结构的过程。程序员这样做是为了更简单、更有效地与网页交互和利用其数据。

## 如何操作：

下面是一个使用库`Gumbo`的简单C编程示例，用于解析HTML。相关代码和输出样例如下：

```C
#include <stdio.h>
#include <stdlib.h>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
        (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("%s\n", href->value);
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<a href='http://example.com'>I'm a link!</a>");
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

输出：

```shell
http://example.com
```

## 深度学习：

解析HTML在网络爬虫和网页分析中曾起到核心作用。早在1990年代的网页出现之初，由于HTML规范并没有完全定义，所以解析HTML是一项相当大的挑战。现在，我们有了不同的工具和库（如上述的Gumbo），使得这个过程变得更容易。

解析HTML有很多方法。例如，你可以使用诸如Python的Beautiful Soup或者JavaScript的Cheerio这样的库。然而，C语言因其执行速度快和内存效率高，通常常常被用于解析大型或复杂的HTML文件。

具体到解析HTML，通常都涉及到一个叫做DOM（文档对象模型）的概念。DOM是一种编程接口，它将标记型语言（例如HTML）按照树状结构表示出来，便于程序员更好地定位和操作其中的元素和属性。

## 更多信息：

如果你对HTML解析或Gumbo库感兴趣，这里有一些有益的链接：
- Gumbo库：https://github.com/google/gumbo-parser
- HTML Parsing基础知识：https://www.w3.org/TR/html52/syntax.html#parsing
- HTML和DOM：https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction