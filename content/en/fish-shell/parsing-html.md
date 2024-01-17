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

## What & Why?
Parsing HTML is the process of analyzing a document written in Hypertext Markup Language (HTML) to extract relevant information and manipulate it as needed. Programmers use it to automate tasks, such as data scraping and web development, by extracting data from HTML documents and making it available for further processing.

## How to:
Fish Shell provides built-in tools for parsing HTML using its `string` and `fetch` commands. Let's take a look at some examples to see how we can use these commands to parse HTML.

```
# Fetch the HTML content from a website
set html (fetch https://www.example.com)

# Extract the title of the webpage
set title (string match --regex "<title>(.+)</title>" $html | string replace --regex --repl='$1')

# Print the extracted title
echo $title
```

In the first line, we use the `fetch` command to retrieve the HTML content from the specified URL and store it in a variable called `html`. Then, we use the `string match` command with a regular expression to extract the webpage's title and save it in the `title` variable. Lastly, we use the `echo` command to print the extracted title.

We can also use these commands to extract other elements from the HTML document, such as images, links, and text. With the power of Fish Shell's string and regular expression capabilities, the possibilities for parsing HTML are endless.

## Deep Dive:
Parsing HTML has been a crucial part of web development and data scraping for decades. In the early years of the internet, programmers had to use complex tools or write scripts in other languages to parse HTML. With Fish Shell, the process has become much simpler and more efficient.

There are other alternatives for parsing HTML, such as using dedicated libraries or tools. However, Fish Shell's built-in commands make it a convenient option, especially for simple and quick HTML parsing tasks.

Behind the scenes, Fish Shell uses the `pcre` library to support regular expressions, making it a powerful tool for parsing HTML. Additionally, the `fetch` command utilizes cURL to fetch the HTML content from a website.

## See Also:
You can find more information about Fish Shell's `string` and `fetch` commands and their options in the official documentation: https://fishshell.com/docs/current/index.html. Additionally, there are many online resources and tutorials available to help you learn more about parsing HTML with Fish Shell.