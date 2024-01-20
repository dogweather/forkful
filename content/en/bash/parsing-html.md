---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML Using Bash

## What & Why?

Parsing HTML refers to breaking down web data into more readable or usable parts. As programmers, we do this to extract specific information from webpages like data scraping.

## How to:

We'll be using the `grep` and `sed` commands in Bash for parsing. Here is an example:

```Bash
curl -s 'http://yourwebsite.com' | grep '<title>' | sed -e 's,<title>,,g' -e 's,</title>,,g'
```

This will print the title of the webpage. Here's what each part does:

- `curl -s 'http://yourwebsite.com'`: Fetches the webpage's HTML.
- `grep '<title>'`: Searches for the line containing the `<title>` tag.
- `sed -e 's,<title>,,g' -e 's,</title>,,g'`: Removes the `<title>` and `</title>` tags.

## Deep Dive

Parsing HTML has been around since the early days of the internet, with several programming languages offering their tools. Bash has `grep` and `sed`, but if you're looking for more powerful alternatives, you might get into a language like Python. Libraries like BeautifulSoup in Python provide more flexible parsing capabilities.

Technical note: this Bash method is somewhat crude, as it only matches the exact lines containing `<title>`. Tools like BeautifulSoup handle nested tags, varying spaces, and other complexities found in real-world HTML.

## See Also

- Bash scripting tutorial: https://ryanstutorials.net/bash-scripting-tutorial/
- BeautifulSoup Python library: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- More about `grep` and `sed`: https://www.gnu.org/software/grep/manual/grep.html and https://www.gnu.org/software/sed/manual/sed.html.