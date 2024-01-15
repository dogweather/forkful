---
title:                "Downloading a web page"
html_title:           "Javascript recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Ever come across a beautiful webpage and thought, "I want to access this offline, without using the internet"? Or maybe you want to scrape and analyze data from a website. Whatever the reason may be, downloading a web page is a valuable skill for any programmer.

## How To
It's actually quite simple to download a web page using Javascript. Just follow these steps:

1. First, we need to make a request to the URL of the webpage we want to download. This can be done using the `XMLHttpRequest` object or the newer `fetch()` method. Let's use `fetch()` for this example:
```Javascript
fetch('https://example.com')
    .then(response => response.text())
    .then(data => console.log(data))
```
2. Once we have the response from the webpage, we can access the HTML content using the `response.text()` method. This returns a promise, so we use another `then()` method to log the HTML content to the console.
3. And that's it! We now have the HTML content of the webpage, which we can save to a file or manipulate as we wish.

## Deep Dive
As mentioned earlier, there are two methods for making a request to a webpage - `XMLHttpRequest` and `fetch()`. Both work similarly and achieve the same result, but `fetch()` is newer and has a cleaner syntax. It also supports promises, making it easier to work with asynchronous code.

Additionally, we can specify options in the `fetch()` method, such as `credentials` (for authentication) and `headers` (to specify the content type). You can read more about these options and how to use them in the [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch) documentation.

It's worth noting that downloading a web page using Javascript is not the same as web scraping. While web scraping involves extracting specific data from a webpage, downloading a webpage simply retrieves the HTML content.

## See Also
- [MDN Web Docs: Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [W3Schools: XMLHttpRequest Object](https://www.w3schools.com/xml/xml_http.asp)
- [Web Scraping vs Web Crawling vs Screen Scraping](https://www.promptcloud.com/blog/web-scraping-web-crawling-screen-scraping/)