---
title:                "ניתוח HTML"
date:                  2024-01-20T15:31:09.974537-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
Parsing HTML means extracting information from HTML documents. Programmers parse HTML to manipulate, scrape, or analyze web data.

## How to: (איך לעשות:)
Fish doesn't have built-in HTML parsing abilities. You'll need external tools. Here’s how to use `pup`, a command-line HTML parser, with Fish.

Install `pup`:
```Fish Shell
sudo apt install pup
```

Suppose we have a file `example.html` with this content:
```html
<ul>
    <li>First Item</li>
    <li>Second Item</li>
</ul>
```

To extract list items:
```Fish Shell
cat example.html | pup 'li text{}'
```

Sample output:
```
First Item
Second Item
```

## Deep Dive (עומק הצלילה)
Fish, being a shell designed for interactive use, isn't intended for HTML parsing. Historically, Unix philosophy suggests using specialized tools in combination. `pup` parses HTML using CSS selectors. Other tools like `xmllint` and `tidy` can serve similar purposes. 

Working with `pup` in Fish is simple due to the piping mechanism, which is a robust Unix feature. This modular approach leverages the power of existing Unix command-line utilities, keeping scripts simple and maintainable.

## See Also (ראה גם)
[Pup GitHub Repository](https://github.com/ericchiang/pup) - Learn more about `pup`.

[HTML Parsing with Python](https://docs.python.org/3/library/html.parser.html) - For a language with built-in support.

[Web Scraping Best Practices](https://www.scrapingbee.com/blog/web-scraping-best-practices/) - To understand the ethics and legalities.
