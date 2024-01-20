---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage is the process of saving a copy of that page's content, often used for offline viewing or data scraping. Programmers often do this for web scraping - extracting structured data from websites for various analyses.

## How to:

Fish, a user-friendly shell, can download a page easily using the in-built function `curl`. Here is a simple example:

```fish
function download_page -a url
  curl $url -o "output.html"
end
```

In the above code, `download_page` is a function that takes in URL as an argument and downloads the webpage to a file named "output.html". Let's download Google's homepage:

```fish
download_page https://www.google.com
```

After running the code, you'd find a file "output.html" in your current directory, containing the HTML content of the page.

**Sample output:**

```fish
% download_page https://www.google.com
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100    14k  100    14k    0     0  94736      0 --:--:-- --:--:-- --:--:-- 94736
```

## Deep Dive:

The `curl` command used above is a versatile tool born in 1997, first introduced to satisfy the need for a command-line HTTP client. Today, it supports numerous protocols, HTTP being one of them.

The `wget` command is another alternative for downloading web pages. It functions similarly to `curl` but differs in some behaviors. For instance, `wget` recursively downloads by default, while `curl` doesn't.

The function downloads the entire webpage as is, including scripts and stylesheets. For more extravagant tasks, like downloading specific elements or handling sessions, more complicated tools like `Beautiful Soup` in Python would be more suitable.

## See Also:

- Fish shell documentation: (https://fishshell.com/docs/current/)
- The manual for `curl`: (https://curl.se/docs/manual.html)
- The manual for `wget`: (https://www.gnu.org/software/wget/manual/wget.html)
- Beautiful Soup for Python: (https://www.crummy.com/software/BeautifulSoup/bs4/doc/)