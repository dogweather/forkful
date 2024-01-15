---
title:                "Downloading a web page"
html_title:           "Python recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Downloading a web page can be a useful skill for a variety of reasons - from web scraping and data analysis to offline reading or saving important information.

## How To

To download a web page in Python, you will need to use the `urllib` library. Here's a simple example of how to download a webpage and print its content:

```python
import urllib.request

# specify the URL of the web page
url = "https://www.example.com"

# use urllib.request to open the URL and read its content
page = urllib.request.urlopen(url).read()

# print the page content
print(page)
```

Output:

```
b'<!doctype html>\n<html>\n<head>\n  <title>Example Domain</title>\n\n  <meta charset="utf-8"/>\n  <meta http-equiv="Content-type" content="text/html; charset=utf-8"/>\n  <meta name="viewport" content="width=device-width, initial-scale=1"/>\n  <style type="text/css">\n  body {\n    background-color: #f0f0f2;\n    margin: 0;\n    padding: 0;\n    font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Open Sans", "Helvetica Neue", "Helvetica", Arial, sans-serif;\n    \n  }\n  div {\n    width: 600px;\n    margin: 5em auto;\n    padding: 2em;\n    background-color: #fdfdff;\n    border-radius: 0.5em;\n    box-shadow: 2px 3px 7px 2px rgba(0,0,0,0.02);\n    \n  }\n' 
```

## Deep Dive

The `urllib` library is used for opening URLs and reading their contents. It has various methods for sending requests and handling responses from the server. Using the `urllib.request` module, we can open a web page and read its contents as shown in the example above. Additionally, we can also specify headers, handle redirects, and handle errors using the `urllib.request.Request` class.

## See Also

Here are some additional resources for learning more about downloading web pages in Python:

- [urllib Documentation](https://docs.python.org/3/library/urllib.html)
- [Web Scraping in Python](https://realpython.com/python-web-scraping-practical-introduction/) article by Real Python
- [Requests Library](https://requests.readthedocs.io/en/master/) for making HTTP requests in Python