---
date: 2024-02-03 19:02:33.698253-07:00
description: "How to: Python provides powerful libraries like BeautifulSoup and requests\
  \ for web scraping and HTML parsing. To begin, you need to install these\u2026"
lastmod: '2024-03-13T22:44:59.706177-06:00'
model: gpt-4-0125-preview
summary: Python provides powerful libraries like BeautifulSoup and requests for web
  scraping and HTML parsing.
title: Parsing HTML
weight: 43
---

## How to:
Python provides powerful libraries like BeautifulSoup and requests for web scraping and HTML parsing. To begin, you need to install these libraries if you haven't already:

```bash
pip install beautifulsoup4 requests
```

Here's a basic example using `requests` to fetch the HTML content of a webpage and `BeautifulSoup` to parse it:

```python
import requests
from bs4 import BeautifulSoup

# Fetch the content of a webpage
URL = 'https://example.com'
page = requests.get(URL)

# Parse the HTML content
soup = BeautifulSoup(page.content, 'html.parser')

# Example of extracting the title of the webpage
title = soup.find('title').text
print(f'Webpage Title: {title}')
```

**Sample output**:
```
Webpage Title: Example Domain
```

For more complex queries, like extracting all links from a webpage, you can use BeautifulSoup's various methods for navigating and searching the parse tree:

```python
# Extract all links within <a> tags
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Sample output**:
```
https://www.iana.org/domains/example
```

BeautifulSoup's flexibility allows you to tailor your search for the exact data needed, making HTML parsing a powerful tool for programmers working with web content.
