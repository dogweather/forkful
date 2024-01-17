---
title:                "Downloading a web page"
html_title:           "Fish Shell recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page simply means retrieving the content present on the website and saving it onto your local computer. Programmers often do this for various reasons, such as analyzing the website's structure, extracting information, or testing their code's functionality.

## How to:

To download a web page using the Fish Shell, we can use the `curl` command. Here's an example:
```Fish Shell
curl https://www.example.com
```
The output of this command would be the HTML content of the website, which we can save into a file using the `-o` flag. Here's a full example:
```Fish Shell
curl -o website.html https://www.example.com
```
This will save the HTML content into a file called `website.html` in the current directory.

To save only a specific portion of the web page, we can use the `grep` command to filter out the desired section. Here's an example:
```Fish Shell
curl https://www.example.com | grep "title"
```
This command will download the webpage and only show the lines containing the word "title."

## Deep Dive:

Downloading web pages using command-line tools has been around since the beginning of the internet. However, with the advancements in web technologies, it has become more common for developers to use client-side scripting languages like JavaScript to render web pages, making it more challenging to retrieve the content using a command-line tool. In such cases, using a headless browser like PhantomJS or Selenium can be an alternative to downloading web pages.

## See Also:

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Curl's website](https://curl.haxx.se/)
- [A Beginner's Guide to Curl](https://blog.runcloud.io/what-is-curl-with-example-commands/)