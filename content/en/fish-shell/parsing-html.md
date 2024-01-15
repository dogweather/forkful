---
title:                "Parsing html"
html_title:           "Fish Shell recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Why

HTML is the standard markup language used for creating web pages. Parsing HTML allows you to extract and manipulate specific data from a webpage, giving you the ability to automate tasks or gather information quickly and efficiently.

## How To

If you're using Fish Shell, you have access to various HTML parsing tools such as `pup`, `hxselect`, and `htmlq` through its package manager, `fisher`. Here's a simple example of using `pup` to parse and extract data from a webpage:

```
Fish Shell
# Install pup using fisher
fisher install jethrokuan/pup

# Define a website URL
set url "https://www.example.com"

# Use pup to parse the webpage and extract a specific element's text
pup 'h1 span' < $url
```

The output of the above code will be the text within the `<h1>` element with a child `<span>` element on the given webpage. You can also use these tools to manipulate HTML and perform other tasks such as web scraping.

## Deep Dive

While there are several HTML parsing tools available for Fish Shell, `pup` is a popular choice due to its simplicity and powerful features. It uses CSS selectors to navigate and search through the HTML structure, making it easy to target specific elements. You can also use it to manipulate HTML attributes and perform more complex operations like filtering, sorting, and even pagination.

HTML parsing is not limited to web pages, you can also use it to extract data from local HTML files or any other source that contains HTML code. This can come in handy when working with data stored in different formats or when trying to automate tasks within a larger program or script.

## See Also

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Official Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
- [pup GitHub repository](https://github.com/ericchiang/pup)
- [hxselect GitHub repository](https://github.com/ericchiang/htmlq)
- [htmlq GitHub repository](https://github.com/koenrh/htmlq)