---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

# Fish Shell and Parsing HTML: A Coding Guide

## What & Why?

Parsing HTML is the process of analyzing a document, typically web pages, to identify its structural and content details. Programmers do it to manipulate, extract or understand data present on a web page.

## How to:

Writing the code in Fish Shell:

```fish
set url "https://example.com"
set content (curl -sL $url)
set dom (echo $content | pup 'html{}')

echo $dom
```

This program fetches a webpage's HTML (using `curl`), then deduces its structure (via `pup`).

Here's a hypothetical output example:

```fish
<html>
  <head>
    <title>Example Website</title>
  </head>
  <body>
    <h1>Welcome to Example.com!</h1>
    <p>This is an example website.</p>
  </body>
</html>
```

Now, if you need to extract specific details like the title tag:

```fish
set title (echo $dom | pup 'title text{}')

echo $title
```

Sample output:
```fish
Example Website
```

## Deep Dive 

Parsing in Fish Shell is a newer development with historical roots. Before libraries like `pup` in Fish Shell, programmers had to wrestle with complicated regex commands or use bulky languages like Python or Java to parse HTML.

Alternatives for `pup` in Fish Shell are syntax parsing libraries like `beautifulsoup4` in Python or `Jsoup` in Java, which offer more intensive parsing operations.

Moreover, `pup` is an example of a basic yet powerful HTML parser. Its easiness can be attributed to its design. In the parsing process, the `pup` command treats the HTML tags like they are part of a file directory, simplifying the commands needed to find specific pieces. 

## See Also

To learn more, see the Fish Shell documentation ([www.fishshell.com/docs/current/index.html](https://fishshell.com/docs/3.1/index.html)) and the `pup` command's GitHub repository ([www.github.com/ericchiang/pup](https://github.com/ericchiang/pup)). For broader context, check out 'HTML Parsing' on Wikipedia ([en.wikipedia.org/wiki/HTML_parsing](https://en.wikipedia.org/wiki/HTML_parsing)).