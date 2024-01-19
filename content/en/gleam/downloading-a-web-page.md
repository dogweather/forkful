---
title:                "Downloading a web page"
html_title:           "Gleam recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page is the act of retrieving all resources of the site, including HTML, CSS, JavaScript, and media files. Programmers do this for offline browsing, site backup, or data scraping, essentially making the web content accessible regardless of network connection.

## How to:
In Gleam, we can use the `reqw` module to make HTTP requests. Here's a Gleam code sample to download a web page:

```Gleam
import gleam/result.{Ok}
import reqw

pub fn main(url: String) {
  let resp = reqw.get(url)

  case resp {
    Ok(response) -> 
      case response.body {
        Ok(body) -> 
          io.println(body)
        Error(reason) -> 
          io.println(reason)
      }
    Error(_reason) -> 
      io.println("Failed to fetch page.")
  }
}
```

Running this code with a valid URL will output the HTML of the requested page. If the URL is not accessible or invalid, it will output an error message.

## Deep Dive
Web page downloading has a rich history, tracing back to the initial days of the internet when offline browsing was a necessity due to expensive and unreliable connections. Alternatives include using web scraping libraries in Python (like BeautifulSoup or Scrapy), or JavaScript API methods (like `fetch`). 

In Gleam, the `reqw` module allows us to make HTTP requests. The HTTP response contains the body that is the HTML of the webpage, which can then be parsed or saved as required. It’s simple and effective but lacks features you might find in more full-featured libraries.

## See Also
- [Gleam `reqw` docs](https://hexdocs.pm/reqw/reqw.html)
- [Python Web Scraping with BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [JavaScript fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)