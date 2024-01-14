---
title:                "Fish Shell recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Downloading a web page is a common task for those who work with web development, automation, or data analysis. It allows you to retrieve HTML, CSS, and JavaScript files from a website and use them for various purposes, such as creating backups, extracting data, or testing web pages.

## How To

To download a web page using the Fish Shell, we can utilize the `curl` command. This command allows us to retrieve data from a URL and store it locally. Let's take a look at a simple example:

```
Fish Shell> curl https://www.website.com > index.html
```

In this example, we are using `curl` to download the homepage of a website and save it as an HTML file called `index.html`. The file will be saved in the current directory where the command is executed.

We can also specify a specific file name and location for our downloaded web page using the `-o` flag:

```
Fish Shell> curl -o /path/to/folder/page.html https://www.website.com
```

We can also use `curl` to download multiple web pages at once by providing a list of URLs:

```
Fish Shell> curl -o /path/to/folder/ https://www.website1.com https://www.website2.com https://www.website3.com
```

The `curl` command also allows us to specify other options, such as authentication, user agents, and headers. To learn more about these options, we can use the `curl --help` command.

## Deep Dive

While `curl` is a powerful tool for downloading web pages, it may not always be the best option. For example, if we want to download a large number of files, using `wget` may be more efficient. Additionally, there are other helpful tools like `httrack` and `scrapy` that provide more robust options for web scraping.

It's also worth noting that if we want to download a web page for offline use, we should consider using a web scraping library such as Python's `BeautifulSoup` or `Scrapy` instead of just saving the HTML file. These libraries allow us to extract specific elements and data from web pages, making it easier to work with the downloaded data.

## See Also

- [Fish Shell documentation: `curl` command](https://fishshell.com/docs/current/cmds/curl.html)
- [Official `curl` website](https://curl.haxx.se/) 
- [Using `wget` for downloading web pages](https://linuxize.com/post/wget-command-examples/)