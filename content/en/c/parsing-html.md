---
title:                "Parsing HTML"
date:                  2024-01-20T15:30:05.167808-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means reading and understanding the structure of HTML documents by a program. Programmers do it to manipulate, extract, or check content, often while scraping websites or processing web data.

## How to:

Alright, let's get to the code. C doesn't have built-in support for HTML parsing, so we'll use a library called Gumbo, which is a pure C HTML5 parser. Here's a quick example:

```C
#include <stdio.h>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
       (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("Link found: %s\n", href->value);
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Example</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Sample output:

```
Link found: https://example.com
```

This example finds 'a' tags and prints out the href attributes. Remember to link against gumbo (`gcc -o example example.c -lgumbo`) and install the library first.

## Deep Dive

The story of HTML parsing in C is a bit rugged. There's no one-size-fits-all solution because HTML is complex and usually not so consistent. Gumbo, which we used, was developed by Google as a part of their open-source projects. It's designed to tolerate real-world messiness of web pages.

Alternatives include libxml2 with an HTML parser mode, though it's historically been more aligned with XML parsing. Another one is htmlcxx which is actually C++, but let's not get sidetracked.

Performance-wise, C parsers can be blazing fast but normally don't offer the ease of use that Python libraries do. When rolling out C for HTML parsing, you're likely after performance, or you're integrating it into an existing C codebase. It can be fiddly, as most C libraries are low-level and more hands-on than Python or JavaScript parsers.

## See Also

- Gumbo Parser: [https://github.com/google/gumbo-parser](https://github.com/google/gumbo-parser)
- libxml2 HTML parser: [http://xmlsoft.org/html/libxml-HTMLparser.html](http://xmlsoft.org/html/libxml-HTMLparser.html)
- htmlcxx: [http://htmlcxx.sourceforge.net/](http://htmlcxx.sourceforge.net/) 
- For a gentle start, consider a tutorial on web scraping with Python using Beautiful Soup or Python's `html.parser` as an easier intro to the subject.
