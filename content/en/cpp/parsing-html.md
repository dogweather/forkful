---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML involves analyzing an HTML document to understand its structure. Programmers parse HTML to access or modify web content programmatically using coding languages like C++.

## How to:

Let's parse HTML using a fantastic lib `gumbo-parser` in C++. The steps might look like:

1. Install 'gumbo-parser': `apt-get install libgumbo-dev`

```C++
#include <stdio.h>
#include <gumbo.h>

void parse_html(const std::string& html)
{
    GumboOutput* output = gumbo_parse(html.c_str());

    // your operations...

    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

2. Use C++ to call `gumbo_parse()`, giving it your HTML. It returns `GumboOutput* output`.
```C++
std::string html = "<html><body>Hello World!</body></html>";
GumboOutput* output = gumbo_parse(html.c_str()); // parse the HTML.
```

3. Traverse the `output` tree to access the parsed HTML content.
```C++
GumboNode* html_node = output->root;
```

Gumbo destroys the output to prevent memory leaks!
```C++
gumbo_destroy_output(&kGumboDefaultOptions, output);
```

Run that C++ code, and HTML is now accessible!

## Deep Dive

Parsing HTML as a practice has been around since the web's inception. HTML's hierarchical tree-like structure makes traversal and content manipulation possible.

`gumbo-parser` is a C implementation by Google, compliant with the HTML5 specification. Alternatively, programmers utilize libs like `htmlcxx` C++ library, the Python-based BeautifulSoup, or `Jsoup` in Java.

Performing HTML parsing in C++ attraction lies in its high-performance potential, however, managing memory manually in C++ can be tricky and lead to potential leaks if not handled correctly.

## See Also

To learn more about parsing HTML in C++ and the gumbo-parser library:

1. Official Documentation: <https://github.com/google/gumbo-parser>
2. HTML parsing in C++: <https://stackoverflow.com/questions/686041/recommendation-for-html-parsing-library-for-c-and-or-c>
3. The gumbo-parser API: <https://docs.rs/crate/gumbo-parser/0.1.4>