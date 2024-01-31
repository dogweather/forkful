---
title:                "解析HTML"
date:                  2024-01-20T15:30:28.615784-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"

category:             "C++"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？

Parsing HTML means extracting data and information from HTML code. Programmers parse HTML to interact with web content, automate data extraction, scrape websites, or to test web applications.

## How to? 怎么做？

Here’s a simple C++ code snippet using the library Gumbo for HTML parsing:

```C++
#include <gumbo.h>
#include <iostream>

void search_for_text(GumboNode* node) {
    if (node->type == GUMBO_NODE_TEXT) {
        std::cout << node->v.text.text << std::endl;
    } else if (node->type == GUMBO_NODE_ELEMENT) {
        GumboVector* children = &node->v.element.children;
        for (unsigned int i = 0; i < children->length; ++i) {
            search_for_text(static_cast<GumboNode*>(children->data[i]));
        }
    }
}

int main() {
    const char* html = "<html><body>Hello, world!</body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_text(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Output:
```
Hello, world!
```

## Deep Dive 深入探讨

Parsing HTML has evolved. In the ’90s, parsing was often done with regular expressions, which led to many problems. Better alternatives like specialized libraries (e.g., Gumbo, HTML Tidy) now exist, which parse HTML into a DOM structure.

The Gumbo parser library is designed by Google. It's excellent for C/C++ and doesn't depend on external libraries. When implementing, manage memory properly. Use `gumbo_destroy_output` to avoid leaks.
 
Alternatives include libraries in other languages, like Beautiful Soup in Python, which offer more features but may lack performance compared to C++ libraries.

## See Also 参考链接

- Gumbo Parser: https://github.com/google/gumbo-parser
- HTML Tidy Project: http://www.html-tidy.org/
- Beautiful Soup Documentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/ 

Remember to choose libraries considering your project needs, performance requirements, and language familiarity. Happy parsing!
