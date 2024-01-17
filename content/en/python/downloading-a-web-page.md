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

## What & Why?

Downloading a web page is the process of retrieving data from a website and saving it onto your local device. Programmers often do this to gather information or use it for further processing in their programs.

## How to:

To download a web page in Python, you can use the `requests` library. First, import the library using the following code:
```
import requests
```
Next, use the `get()` method to retrieve the web page. Simply pass in the URL of the page you want to download as an argument:
```
url = 'https://www.example.com'
response = requests.get(url)
```
To view the contents of the downloaded page, you can use the `text` attribute:
```
print(response.text)
```
This will print out the HTML code of the web page. You can also save the contents to a file on your device by using the `content` attribute:
```
with open('page.html', 'wb') as file:
    file.write(response.content)
```
This will save the web page as a local HTML file named "page.html".

## Deep Dive:

The process of downloading web pages has evolved over the years, with different methods and tools being used. One of the first methods was using the `urllib` library, which is still available in Python but has been replaced by alternative libraries like `requests` which offer more features and better performance.

In addition to using libraries, there are also command-line tools like `curl` and `wget` that can be used to download web pages. However, these tools may not have the flexibility and customization options that a programming language like Python offers.

When downloading a web page, it's important to consider factors like efficiency, handling errors, and proper handling of data. Libraries like `requests` have built-in features to handle these issues, making the process smoother and more reliable.

## See Also:

- [Documentation for `requests` library](https://requests.readthedocs.io/en/master/)
- [Comparison of `requests` and `urllib`](https://stackoverflow.com/questions/2018026/what-are-the-differences-between-the-urllib-urllib2-and-requests-module)
- [Official Python documentation for `urllib`](https://docs.python.org/3/library/urllib.html)