---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML with Python: A Pragmatic Approach

## What & Why?

Parsing HTML is the process of making sense of the content in an HTML document. It's a common task for programmers for various reasons, such as web scraping, testing, and automating tasks on websites.

## How to:

Let's use Beautiful Soup, a popular Python library due to its ability to simplify HTML parsing.

Install it via pip:

```Python
pip install beautifulsoup4
```

And here's a basic example of using it to extract all links from a web page:

```Python
from bs4 import BeautifulSoup
import requests

# get HTML content
response = requests.get('https://www.example.com')
html = response.text

# parse HTML
soup = BeautifulSoup(html, 'html.parser')

# find and print all links
for link in soup.find_all('a'):
    print(link.get('href'))
```

If you run this script with a valid URL, it'll print out all the links found on the page.

## Deep Dive

Despite its simplicity, HTML parsing has a rich history and alternatives. The use of regular expressions was a common practice before dedicated libraries emerged. This method, while working for simple use cases, fails with complex HTML and isn't as efficient nor readable as using a parser like Beautiful Soup.

Alternatives to Beautiful Soup include lxml and html5lib, which also provide APIs for HTML parsing. Fairly speaking, lxml is significantly faster, but Beautiful Soup is more lenient with error handling and offers a few more features.

How these parsers work internally varies. Some, like lxml, compile to C code for maximum speed, while others, like Beautiful Soup, are pure Python and emphasize on the ease of use.

## See Also

Beyond the basic use case presented, there's a lot more that can be done with HTML parsing. Take a look at these resources for a fuller picture:

- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [LXML's Parsing Tutorial](https://lxml.de/parsing.html)
- [Real Python's Tutorial on Web Scraping](https://realpython.com/python-web-scraping-practical-introduction/)
- [W3School's HTML Tutorial](https://www.w3schools.com/html/default.asp)