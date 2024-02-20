---
date: 2024-02-03 19:02:45.946647-07:00
description: "Parsing HTML is about extracting data or information from HTML content,\
  \ a common task when dealing with web data. Programmers do this to automate the\u2026"
lastmod: 2024-02-19 22:05:18.932655
model: gpt-4-0125-preview
summary: "Parsing HTML is about extracting data or information from HTML content,\
  \ a common task when dealing with web data. Programmers do this to automate the\u2026"
title: Parsing HTML
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is about extracting data or information from HTML content, a common task when dealing with web data. Programmers do this to automate the extraction of information from websites, for tasks such as web scraping, data mining, or automated testing.

## How to:

Fish shell, predominantly, is not designed for parsing HTML directly. However, it excels in gluing together Unix tools like `curl`, `grep`, `sed`, `awk`, or using specialized tools like `pup` or `beautifulsoup` in a Python script. Below are examples that showcase how to leverage these tools from within Fish shell to parse HTML.

### Using `curl` and `grep`:
Fetching HTML content and extracting lines that contain links:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Output:
```
/page1.html
/page2.html
...
```

### Using `pup` (a command-line tool for parsing HTML):

First, ensure `pup` is installed. Then you can use it to extract elements by their tags, ids, classes, etc.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Output, similar to the `grep` example, would list href attributes of `<a>` tags.

### With a Python script and `beautifulsoup`:

While Fish itself can't parse HTML natively, it seamlessly integrates with Python scripts. Below is a concise example that uses Python with `BeautifulSoup` to parse and extract titles from HTML. Ensure you have `beautifulsoup4` and `requests` installed in your Python environment.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Usage:

```fish
parse_html 'https://example.com'
```

Output:
```
Example Domain
```

Each of these methods serves different use cases and scales of complexity, from simple command-line text manipulation to the full parsing power of `beautifulsoup` in Python scripts. Depending on your needs and the complexity of the HTML structure, you may choose a straightforward Unix pipeline or a more powerful scripting approach.
