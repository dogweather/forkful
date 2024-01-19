---
title:                "Parsing html"
html_title:           "Python recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML in Python: A Brief Guide

## What & Why?
Parsing HTML refers to breaking down a web page into manageable pieces, essentially making sense of its structure. Programmers parse HTML to extract useful data, automate browser tasks or build web scrapers.

## How to:

Python provides several libraries for HTML parsing. For simplicity, our tutorial uses `BeautifulSoup4`. To install it, run:

```Python
pip install beautifulsoup4
```

Let's say you have a simple HTML file named `test.html`.

```HTML
<html>
    <head>
        <title>Test Page</title>
    </head>
    <body>
        <h1>Welcome to the Test Page</h1>
        <p>This is a simple paragraph.</p>
    </body>
</html>
```

You can open and parse it with BeautifulSoup as follows:

```Python
from bs4 import BeautifulSoup

with open('test.html', 'r') as html_file:
    soup = BeautifulSoup(html_file, 'html.parser')

print(soup.prettify())
```

The output will retain the HTML structure:

```HTML
<html>
 <head>
  <title>
   Test Page
  </title>
 </head>
 <body>
  <h1>
   Welcome to the Test Page
  </h1>
  <p>
   This is a simple paragraph.
  </p>
 </body>
</html>
```

To extract text from the `<h1>` tag:

```Python
header = soup.find('h1').text
print(header)
```

The output will be:

```
Welcome to the Test Page
```

## Deep Dive

HTML parsing in Python dates back to the early days of the web. Developers needed a way to pull data from the once-static web.

Alternatives to BeautifulSoup include `lxml` and `html.parser`. BeautifulSoup is comprehensive and forgiving with broken HTML. `lxml`, being written in C, is faster. `html.parser`, part of Python's standard library, needs no additional installation.

BeautifulSoup transforms complex HTML into a tree of Python objects, like tags, navigable strings, or comments. It's an actual parser, not just a regular expression wrapper. This feature allows it to handle inconsistencies and brokenness in the HTML.

## See Also

For further reading on BeautifulSoup, check out the [official documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/). An alternative Python library for parsing HTML is [lxml](https://lxml.de/). If you're wondering why not to parse HTML with regex, see this famous [Stack Overflow post](http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454).