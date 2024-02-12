---
title:                "解析HTML"
aliases: - /zh/cpp/parsing-html.md
date:                  2024-02-03T19:11:39.917286-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为何？
解析 HTML 意味着将 HTML 内容分解成程序可以理解和操纵的内容。编程人员这样做是为了提取数据、操纵内容或将网页爬取整合到他们的应用程序中。

## 如何操作：
C++ 并不自带 HTML 解析功能。你通常会使用像 Google 的 Gumbo-parser 或类似的库。这里是使用 Gumbo-parser 的一个快速示例：

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

示例输出：
```
https://example.com
```

## 深入探讨
在 C++ 中解析 HTML 并不总是直截了当的。从历史上看，程序员会使用正则表达式或手写解析器，这两种方式都容易出错且麻烦。如今，像 Gumbo-parser 这样的强大库处理解析的复杂性，让操作变得更简单也更可靠。

替代方案包括 Tidy、MyHTML，或者甚至通过 C++ 的 `system` 函数或嵌入式解释器与 Python 的 BeautifulSoup 集成。

在实现上，这些库将 HTML 转换为文档对象模型（DOM）树。遍历和操作 DOM 允许用户提取和处理数据，如“如何操作”部分所示。

## 另请参阅
- [Gumbo-parser GitHub 仓库](https://github.com/google/gumbo-parser)
- [HTML 解析库列表](https://en.cppreference.com/w/c/experimental/dynamic)
- [C++ 与 Python 互操作性](https://docs.python.org/3/extending/embedding.html)
