---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:39.917286-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C++ \u5E76\u4E0D\u81EA\u5E26 HTML \u89E3\
  \u6790\u529F\u80FD\u3002\u4F60\u901A\u5E38\u4F1A\u4F7F\u7528\u50CF Google \u7684\
  \ Gumbo-parser \u6216\u7C7B\u4F3C\u7684\u5E93\u3002\u8FD9\u91CC\u662F\u4F7F\u7528\
  \ Gumbo-parser \u7684\u4E00\u4E2A\u5FEB\u901F\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:48.108006-06:00'
model: gpt-4-0125-preview
summary: "C++ \u5E76\u4E0D\u81EA\u5E26 HTML \u89E3\u6790\u529F\u80FD\u3002\u4F60\u901A\
  \u5E38\u4F1A\u4F7F\u7528\u50CF Google \u7684 Gumbo-parser \u6216\u7C7B\u4F3C\u7684\
  \u5E93\u3002\u8FD9\u91CC\u662F\u4F7F\u7528 Gumbo-parser \u7684\u4E00\u4E2A\u5FEB\
  \u901F\u793A\u4F8B\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

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
