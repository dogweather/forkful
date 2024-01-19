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

Downloading a webpage means making a request to a web server for a specific page just like you would from a browser, but with code. Programmers do it to automate data extraction, run tests, track site changes, or store a local copy of the page.

## How to:

```python
import requests

url = "http://example.com"
response = requests.get(url)

print("Page Download Complete!")

# Save the content
with open("output.html", "w", encoding='utf-8') as f:
    f.write(response.text)
```

This`requests.get(url)` fetches the page at your given URL. Post-fetch, save the content to "output.html". When you run this, you'll see "Page Download Complete!" on the console.

## Deep Dive

Fetching webpages programmatically was not commonplace until Websites became data-rich. It was when websites grew complex and started using databases, API requests, and server-side rendering that programmers began to download pages for further processing. 

For the above code, Python has alternatives. You can use http.client or urllib.request libraries for the same. Important detail: `requests.get(url)` follows redirects by default unlike its alternatives.

## See Also

For more extensive web scraping beyond simple downloads, check out libraries like Beautiful Soup ([Beautiful Soup Doc](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)), Scrapy ([Scrapy Tutorial](https://docs.scrapy.org/en/latest/intro/tutorial.html)) and Selenium ([Selenium Doc](https://selenium-python.readthedocs.io/)). 

For advanced HTTP client features, check out the full `requests` library documentation ([requests Doc](https://docs.python-requests.org/en/latest/)).