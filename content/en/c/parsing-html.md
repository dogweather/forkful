---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML refers to the process of breaking down HTML code into its individual elements, such as tags, attributes, and content. Programmers do this to interact, search, or manipulate web content programmatically, like web scraping, DOM manipulation, etc.

## How to:

Below, you'll find a basic C code snippet that uses the Gumbo parser API to parse an HTML file.

```C

#include <stdio.h>
#include <gumbo.h>

void parse_node(GumboNode* node) {
    if (node->type == GUMBO_NODE_ELEMENT) {
        // Get the node's tag name
        const char* tag_name = gumbo_normalized_tagname(node->v.element.tag);
        printf("Tag: %s\n", tag_name);
    }

    // Recursively traverse the DOM tree
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        parse_node(children->data[i]);
    }
}

int main() {
    FILE* fp = fopen("sample.html", "r");
    char buffer[2048];
    
    // Read file into buffer and parse it
    size_t read_bytes = fread(buffer, 1, sizeof(buffer), fp);
    GumboOutput* output = gumbo_parse_with_options(
        &kGumboDefaultOptions, buffer, read_bytes);

    // Process nodes
    parse_node(output->root);

    // Cleanup
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    fclose(fp);

    return 0;
}

```
## Deep Dive

C was chosen for HTML parsing due to the language's details control over memory which is crucial to manage large amounts of HTML data. A popular library for parsing HTML in C is "Gumbo" created by Google.

Considering alternatives, there are many high-level language libraries such as BeautifulSoup (Python) and JSoup (Java). These are easier to use but offer less control over memory and performance than C.

In the given example, we use Gumbo API's parse feature. It represents HTML documents as tree structures called Document Object Models (DOM). Each HTML tag has an equivalent in the DOM, and we extract the tags one by one.

## See Also

Check out these sources and references to learn more:

1. ["Parsing HTML: The Basics" (MDN)](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML/Getting_started)
2. ["libxml - HTML parsing library written in C" (GNOME)](http://xmlsoft.org/html/libxml-HTMLparser.html)
3. ["BeautifulSoup Documentation" (Python)](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
4. ["HtmlAgilityPack .NET library to parse HTML files" (C#)](http://html-agility-pack.net)
5. ["Gumbo - An HTML5 parsing library in C"](https://github.com/google/gumbo-parser)