---
title:                "Parsing html"
html_title:           "C recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML is the act of extracting and interpreting information from a raw HTML code. Programmers do it to turn messy chunks of code into structured data that can be easily understood and manipulated by computers, thus facilitating tasks such as web scraping and data extraction.

## How to:
To parse HTML in C, we can use the popular library called libxml2. Here's a simple code snippet that shows how to parse a HTML document and print out the extracted information:
```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    FILE *htmlFile = fopen("index.html", "r");
    if (!htmlFile) {
        printf("Error opening file!");
        return 1;
    }

    htmlDocPtr doc = htmlReadFile("index.html", NULL, HTML_PARSE_NOWARNING | HTML_PARSE_NOERROR);
    if (!doc) {
        printf("Error parsing file!");
        return 1;
    }

    htmlNodePtr body = xmlDocGetRootElement(doc);
    if (!body) {
        printf("Error retrieving root element!");
        xmlFreeDoc(doc);
        return 1;
    }

    xmlChar *bodyContent = xmlNodeGetContent(body);
    printf("%s\n", (char *)bodyContent);

    xmlFreeDoc(doc);
    xmlCleanupParser();
    fclose(htmlFile);

    return 0;
}
```
**Output:**
```
<!DOCTYPE html>
<head>
<title>Sample Page</title>
</head>
<body>
<p>Hello world!</p>
</body>
</html>
```

## Deep Dive:
Parsing HTML has been a crucial task for web developers since the early days of the internet. As web pages became more complex and the need for data extraction increased, specialized libraries and tools were developed to make the process easier. One alternative to libxml2 is the HTML parser included in the GNU C Library, which follows a different approach by directly parsing text instead of using a document object model (DOM) like libxml2.

When parsing HTML, it's important to consider the structure and hierarchy of the document, as well as any possible errors or typos in the code. Using an HTML parser library, such as libxml2, takes care of these complexities and allows programmers to focus on manipulating the extracted data.

## See Also:
- [libxml2 documentation](http://www.xmlsoft.org/html/index.html)
- [GNU C Library documentation](https://www.gnu.org/software/libc/manual/html_node/HTML.html)
- [Using C to Write HTML Parser](https://medium.com/@powersuper999/using-c-to-write-html-parser-2e2adc7fb430)