---
title:                "Parsing html"
html_title:           "Bash recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of analyzing HTML code in order to extract information from it. Programmers often do this in order to scrape data from websites or to manipulate the HTML in some way.

## How to:

To parse HTML in Bash, you can use the `wget` and `pup` command line tools. First, install them using your package manager. 

Next, use the `wget` command to download the HTML file you want to parse. For example: 
```Bash
wget https://www.example.com
```
This will save the HTML code in a file called `index.html`.

Then, use `pup` to parse the HTML. For example, to extract all hyperlinks on the page: 
```Bash
pup 'a attr{href}' < index.html
```
This will output a list of all the links found on the page.

If you want to manipulate the HTML in some way, you can use `sed` or `awk` to search and replace specific sections of the HTML code.

## Deep Dive:

Parsing HTML has been a common practice since the early days of the internet. In the past, it was often done manually by web developers. However, as websites became more complex and the amount of data available online grew, the need for automated HTML parsing tools became apparent.

While Bash may not be the most commonly used language for parsing HTML, it is a versatile and powerful tool for many tasks. Other popular alternatives for parsing HTML in the command line include `grep` and `awk` which are also capable of extracting specific data from HTML code.

When using `pup` to parse HTML, keep in mind that it utilizes CSS selectors to specify which elements you want to extract. So some basic knowledge of CSS may come in handy.

## See Also:

- [Wget documentation](https://www.gnu.org/software/wget/manual/wget.html)
- [Pup documentation](https://github.com/ericchiang/pup) 
- [Parsing HTML with grep and sed](https://linuxconfig.org/how-to-parse-data-from-html-into-a-bash-script)