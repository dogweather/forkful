---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
HTML解析是一种处理并理解HTML文档的过程，从而使编程者可以对其进行操作和处理。我们程序员之所以要做这个，主要是因为要处理网页数据，从HTML代码中提取有用的信息。

## 如何操作：
在C++中，我们可以使用著名的库Gumbo来解析HTML。下面是一个简单的例子。

```C++
#include <iostream>
#include "gumbo.h"

static void search_for_links(GumboNode* node) {
  if (node->type != GUMBO_NODE_ELEMENT) {
    return;
  }
  GumboAttribute* href;
  if (node->v.element.tag == GUMBO_TAG_A &&
      (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
    std::cout << href->value << "\n";
  }
  GumboVector* children = &node->v.element.children;
  for (unsigned int i = 0; i < children->length; ++i) {
    search_for_links(static_cast<GumboNode*>(children->data[i]));
  }
}

int main() {
  GumboOutput* output = gumbo_parse("<a href='http://www.google.com'>Google</a>");
  search_for_links(output->root);
  gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

这段代码会输出：

```
http://www.google.com
```

## 深入探讨：
HTML解析历来是网页抓取、内容提取和自动化测试的关键步骤。这个过程可以追溯到90年代，随着网络和网页的发展，需求逐渐增加。虽然选择多样，但C++的Gumbo库因其高效率和良好的错误处理，被广泛应用在各种大型项目中。

在实施细节方面，HTML解析首先是词法分析，将输入的HTML字符串划分为一系列的令牌。然后，解析器根据这些标记创建DOM树。此过程称为“树构筑”。

处理可能出现的HTML语法错误也是解析过程的重要部分。例如，如果存在未关闭的标签，解析器通常会自动修复。

## 参见：
1. [Gumbo文档](https://github.com/google/gumbo-parser)
2. [W3C对HTML解析的规定](https://html.spec.whatwg.org/multipage/parsing.html)
3. [HTML解析的维基百科](https://en.wikipedia.org/wiki/HTML_parsing)