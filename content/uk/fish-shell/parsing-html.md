---
title:                "Парсинг HTML"
date:                  2024-01-20T15:31:29.732986-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Parsing HTML means extracting data from HTML code. Programmers do it to use web content in their applications or to automate browsing tasks.

## How to / Як це зробити
Fish doesn't have built-in HTML parsing, but you can use external tools like `pup`, `hxselect`, or `xmllint`. Here's an example with `pup`.

```Fish Shell
# Install 'pup'
sudo apt install pup

# Fetch HTML and parse for titles
curl -s http://example.com | pup 'h1 text{}'

# Sample output
Example Domain
```

To select elements with `xmllint`, try this:

```Fish Shell
# Install 'xmllint'
sudo apt install libxml2-utils

# Parse HTML, get titles
curl -s http://example.com | xmllint --html --xpath '//h1/text()' - 2>/dev/null

# Sample output
Example Domain
```

## Deep Dive / Поглиблений Аналіз
HTML parsing has roots in data scraping, going back to the early days of the web. `pup` is a command-line tool that processes HTML like `jq` does for JSON. `hxselect` is part of the HTML-XML-utils, ideal for HTML handling. `xmllint`, part of libxml, targets XML but also parses HTML.

Remember, parsing complex HTML only with regex is fragile—there's more to HTML's structure than regex can reliably handle. Tools like `pup` are specialized for this task.

When implementing, consider efficiency. Fetch only necessary HTML. Limit calls to external commands within Fish to reduce overhead. Parsing HTML in Fish often means delegating to these tools and processing the results.

## See Also / Дивіться Також
- `pup` documentation: https://github.com/EricChiang/pup
- HTML-XML-utils: https://www.w3.org/Tools/HTML-XML-utils/
- xmllint: http://xmlsoft.org/xmllint.html
- Fish Shell scripting tutorial: https://fishshell.com/docs/current/index.html