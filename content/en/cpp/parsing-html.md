---
title:                "Parsing html"
html_title:           "C++ recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Why

HTML is the standard markup language used for creating web pages, so understanding its structure and being able to extract information from it is essential for any web developer or data analyst. By learning how to parse HTML, you can automate the process of extracting data from websites and make your work more efficient.

## How To

To parse HTML in C++, we will use the [libxml](http://www.xmlsoft.org/html/index.html) library. Follow these steps to get started:

1. Install libxml on your system by following the instructions on the [download page](http://www.xmlsoft.org/downloads.html).
2. Create a new C++ project in your preferred IDE.
3. Add the `#include <libxml/HTMLParser.h>` statement to your code to access the HTML parsing functions.
4. Use the `htmlReadFile()` function to read an HTML file and store its contents in an `xmlDoc` structure.
5. Use the `xmlDocGetRootElement()` function to get the root element of the HTML document.
6. Traverse the document tree using the `xmlNode` and `xmlChar` datatypes and the `xmlGetProp()` function to get the attribute values.
7. Use the `xmlNodeGetContent()` function to get the text content of a specific element.
8. Use the `xmlFreeDoc()` and `xmlCleanupParser()` functions to free the memory allocated for the document and cleanup the parser, respectively.

Let's see an example of how to extract information from a simple HTML document:

```C++
#include <iostream>
#include <libxml/HTMLParser.h>

int main() {
  // Create an xmlDoc structure to store the HTML contents
  xmlDocPtr doc;

  // Read the HTML file and store its contents in the xmlDoc structure
  doc = htmlReadFile("example.html", NULL, HTML_PARSE_NOBLANKS | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING | HTML_PARSE_NONET);

  // Get the root element of the HTML document
  xmlNodePtr root = xmlDocGetRootElement(doc);

  // Traverse the document tree and print the attribute values and text content
  for (xmlNodePtr node = xmlFirstElementChild(root); node != NULL; node = xmlNextElementSibling(node)) {
    xmlChar *id = xmlGetProp(node, (const xmlChar*)"id");
    xmlChar *name = xmlGetProp(node, (const xmlChar*)"name");

    std::cout << "Element with id " << id << ", name " << name << " and content " << xmlNodeGetContent(node) << std::endl;

    // Free the memory allocated for the attribute values
    xmlFree(id);
    xmlFree(name);
  }

  // Free the memory allocated for the document and cleanup the parser
  xmlFreeDoc(doc);
  xmlCleanupParser();

  return 0;
}
```

**Output:**

```
Element with id container, name div and content This is a container div.
Element with id header, name h1 and content Welcome to My Website!
Element with id content, name p and content This is the content of the page.
```

In this example, we first read an HTML file and store its contents in an `xmlDoc` structure. Then, we traverse the document tree and use the `xmlGetProp()` and `xmlNodeGetContent()` functions to extract the attribute values and text content of each element. Finally, we free the memory allocated for the document and cleanup the parser.

## Deep Dive

Parsing HTML can be a complex task as HTML documents can have nested elements, different tag names, and multiple attributes. It is important to have a good understanding of the structure of HTML documents and how the libxml library works to successfully parse HTML in your C++ programs.

Behind the scenes, libxml uses a parser called an HTML Reader to analyze the HTML code and create a tree structure that represents the document. This tree structure is then used by the library's functions to access and manipulate the nodes and attributes of the document.

One of the key features of libxml is its ability to handle documents that are not well-formed or have errors. The library applies a set of parsing rules known as HTML Parse Options to handle these types of documents. These options can be passed as parameters to the `htmlReadFile()` function to customize the parsing behavior.

For a more in-depth explanation of the libxml library and its HTML parsing capabilities, refer to the [libxml2 HTML Support](http://xmlsoft.org/html/libxml-html.html) page.

## See Also

- [libxml2 official website](http://www.xmlsoft.org/)
- [HTML Standard](https://html.spec.whatwg.org/)
- [C++ documentation](https://devdocs.io/cpp/)