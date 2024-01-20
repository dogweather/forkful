---
title:                "Parsing html"
date:                  2024-01-20T15:31:22.176142-07:00
html_title:           "Bash recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML is the process of decoding the web's lingua franca to sift data or manipulate content. Programmers parse HTML to automate web scraping, integrate APIs, or convert data formats.

## How to:
Fish Shell isn't the go-to for parsing HTML, but with the right tools, it's doable. Let's pull in `pup`, a command-line HTML parser, to work with HTML content.

```fish
# First, install pup
brew install pup

# Fetch the title from example.com
curl -s http://example.com | pup 'title text{}'

# Sample output should be the website's title, something like:
# Example Domain
```

Now let's snag all the hyperlinks:

```fish
# Extract links (href attributes) from example.com
curl -s http://example.com | pup 'a attr{href}'

# Sample output:
# http://www.iana.org/domains/example
```

## Deep Dive
Before Fish Shell and `pup`, folks would use clunky regex or complex server-side scripts. Tools like `pup` smartened the process, leaning on CSS-selector syntax for more intuitive and reliable parsing.

Alternatives include Python's Beautiful Soup or Node.js with Cheerio; they're more powerful but not as concise for one-liners.

Parsing HTML with Fish boils down to outsourcing the task to specialized tools due to its limited text manipulation capabilities. Fish calls out to these tools, captures their output, and lets you work your scripting magic.

## See Also
- [Pup GitHub Repo](https://github.com/ericchiang/pup) - Documentation and examples.
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html) - Learn more about Fish.
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) - For more complex HTML parsing in Python.
- [Cheerio GitHub Repo](https://github.com/cheeriojs/cheerio) - For those interested in a JavaScript-based approach.