---
title:                "ウェブページのダウンロード"
html_title:           "Fish Shell: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Web scraping, or the act of downloading and extracting data from a website, is a common practice for a variety of reasons such as data analysis, market research, and content aggregation. By learning how to use Fish Shell for web scraping, you can easily gather information from websites and automate tasks.

## How To
To download a webpage using Fish Shell, you can use the built-in `curl` command. Simply specify the URL of the webpage you want to download and the output file name using the `> ` symbol. For example:

```Fish Shell
curl https://example.com > example.html
```

This will download the webpage and save it as a HTML file in your current directory. You can also use `curl` to append the downloaded content to an existing file by using `>> `.

If you want to specify a different output directory, you can use the `--output` or `-o` flag followed by the desired directory path.

```Fish Shell
curl https://example.com --output ~/Desktop/example.html
```

You can also use `curl` to follow redirects and download all resources (such as images and scripts) linked on the webpage. This can be done with the `--location` or `-L` flag.

To scrape specific data from a webpage, you can use the `grep` command to filter the downloaded content. For example, if you want to extract all email addresses from a webpage, you can use the HTML tag `mailto` in combination with `grep`.

```Fish Shell
curl https://example.com | grep -o 'mailto:[^"]*'
```

This will print out a list of all the email addresses found on the webpage.

## Deep Dive
In addition to `curl`, Fish Shell also has the `wget` command which can be used for downloading webpages. However, `wget` is not as flexible as `curl` and may not work as well for certain situations. It is recommended to use `curl` for most web scraping tasks.

To make downloading multiple pages easier, you can also create a Fish Shell function or script that uses a for loop to automatically download each page. You can also use a variety of other commands and tools in combination with Fish Shell to automate and manipulate the downloaded data.

## See Also
- [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Cheat Sheet for Fish Shell](https://devhints.io/fish)