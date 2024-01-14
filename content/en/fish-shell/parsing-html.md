---
title:                "Fish Shell recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML, the language used to create web pages, can be incredibly useful for various purposes. For example, it can help extract specific data from a website, automate tasks, or create customized web scraping tools. In this blog post, we will explore how to parse HTML using Fish Shell, a user-friendly and powerful command-line shell.

## How To

To start parsing HTML with Fish Shell, we first need to install the HTML-xml-parser plugin.

```
fisher install jethrokuan/html-xml-parser
```

Once we have the plugin installed, we can use the `hxnormalize` command to convert the HTML to a well-formed XML document. Let's say we want to parse a website's title tag, we can use the following command:

```
hxnormalize -xL url_to_website | hxselect -c 'title'
```

This command will output the title of the website. We can also use CSS selectors to target specific elements on the page. For example, if we want to extract all the links from a webpage, we can use the following command:

```
hxnormalize -xL url_to_website | hxselect -S 'a'
```

This will give us a list of all the links on the webpage.

## Deep Dive

HTML parsing in Fish Shell is carried out using the HTML-xml-parser plugin, which is a wrapper around the `libxml` library. This library is responsible for converting the HTML into a well-formed XML document and providing methods for accessing specific elements on the page.

Fish Shell provides us with the `hxnormalize` and `hxselect` commands to interact with the `libxml` library. The `hxnormalize` command takes in the raw HTML and converts it into a proper XML document, while the `hxselect` command allows us to target specific elements using CSS selectors.

It is also worth mentioning that the HTML-xml-parser plugin supports various other commands such as `hxunent` for unescaping HTML entities and `hxpipe` for formatting the XML output.

## See Also

- [HTML-xml-parser plugin](https://github.com/jethrokuan/html-xml-parser)
- [Official documentation for hxnormalize and hxselect commands](https://www.w3.org/Tools/HTML-XML-utils/doc/)
- [Introduction to CSS selectors](https://www.w3schools.com/cssref/css_selectors.asp)