---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the process of retrieving the HTML code of a specific webpage from the internet. Programmers often do this in order to extract data or information from the webpage, which can then be used for a variety of purposes such as web scraping or data analysis.

## How to:

To download a web page using Bash, we can use the `curl` command. Here's an example of how to download the HTML code of Google's homepage:

```Bash
curl https://www.google.com
```

This will return the HTML code of Google's homepage as the output of the command. We can then save this output to a file by using the `-o` flag followed by the name of the file we want to save it as:

```Bash
curl -o google.html https://www.google.com
```

We can also specify the location where we want to save the file by providing the full path instead of just the file name.

## Deep Dive:

In the early days of the internet, downloading web pages was a slow and cumbersome process. But with the development of faster internet speeds and advanced technologies, this process has become much more efficient and streamlined.

Besides `curl`, there are several other alternatives for downloading web pages in Bash such as `wget` and `lynx`. These commands offer similar functionalities with slight differences in their usage and capabilities.

Behind the scenes, downloading a web page involves making a HTTP request to a web server and receiving a response containing the HTML code. This process is handled by protocols like TCP/IP and HTTP.

## See Also:

- [Curl man Page](https://curl.se/docs/)
- [Wget man Page](https://www.gnu.org/software/wget/)
- [Lynx Homepage](https://lynx.browser.org/)