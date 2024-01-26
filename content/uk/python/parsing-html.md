---
title:                "Парсинг HTML"
date:                  2024-01-20T15:33:35.284486-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Що та чому?)
Parsing HTML means you're extracting useful info from web pages. Programmers parse HTML to automate data collection or interact with websites programmatically.

## How to: (Як зробити:)
To parse HTML in Python, `BeautifulSoup` from `bs4` is a great tool. You'll also need `requests` to fetch webpage content. If you haven't installed these, do it with: `pip install beautifulsoup4 requests`.

```Python
from bs4 import BeautifulSoup
import requests

# Fetch the webpage
response = requests.get('http://example.com')
html_doc = response.text

# Parse the HTML
soup = BeautifulSoup(html_doc, 'html.parser')

# Find data within the HTML
title = soup.find('title').get_text()
print(f'Page Title: {title}')
# Find all links
links = [a['href'] for a in soup.find_all('a', href=True)]
print(f'Links: {links}')
```

Sample output might be:

```
Page Title: Example Domain
Links: ['https://www.iana.org/domains/example']
```

## Deep Dive (Поглиблений аналіз)
HTML (HyperText Markup Language) structures content on the web. Parsing HTML has been a thing since early web days, manually at first, then via various libraries. `BeautifulSoup` stands out due to its ease of use and powerful features. It handles different parsers, like `html.parser` for simple cases, or `lxml` for speed. 

Alternatives to `BeautifulSoup` include `lxml` directly, or even `PyQuery` if you prefer a jQuery-like syntax. Implementation-wise, remember that web pages can change – your parsing code might break if the structure of the HTML it relies on changes.

## See Also (Дивіться також)
- BeautifulSoup documentation: https://beautiful-soup-4.readthedocs.io/en/latest/
- Requests library documentation: https://requests.readthedocs.io/en/master/
- Web scraping guide with Python: https://realpython.com/beautiful-soup-web-scraper-python/
