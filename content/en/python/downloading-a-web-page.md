---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage essentially means fetching its HTML content, which is extremely useful for tasks such as data scraping and testing. Programmers perform this task for web automation, data mining, and testing.

## How to:

Python, with its wide variety of libraries, makes downloading a webpage quite simple. We will be using the `requests` library for this task.

Here's how you do it:

```Python
import requests
  
# Making a GET request
r = requests.get('https://www.google.com')
  
# check status code for response received
# success code - 200 
print(r)
  
# print content of request
print(r.json())
```

This code sends a GET request to Google's home page and prints the returned HTML content.

For our example, the output might be something like:

```Python
<Response [200]>
<!doctype html><html itemscope="" ...
```

## Deep Dive

Historically, web scraping or downloading a webpage was a laborious task involving raw HTTP requests and HTML parsing. Libraries like `requests` and `BeautifulSoup` simplified the process in Python.

You're not restricted to `requests`. Alternatives libraries like `urllib` and `httplib` are also widely used. Each has its unique features, so choose based on your specific requirement.

When you're downloading a web page, what actually happens is that you make a GET request to the server hosting that page. The server responds with the HTML content of the page. It is this HTML content that we refer to as "downloading a webpage".

## See Also

Check the following Python libraries for more details:
* [Requests](https://docs.python-requests.org/en/master/)
* [Urllib](https://docs.python.org/3/library/urllib.html)
* [Httplib](https://docs.python.org/3/library/http.client.html)

Scrapy is another popular library used for web crawling and scraping. Visit the link for more information:
* [Scrapy](https://scrapy.org/)

This [Python Web Scraping Tutorial](https://www.datacamp.com/community/tutorials/web-scraping-using-python) provides further insights on the topic.