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

# Downloading Web Pages in Gleam

## What & Why?

Downloading a web page in programming is the act of retrieving the content of a web page from the internet. Programmers do this to incorporate data from websites into their own programs, or to scrape information for analysis or automation purposes.

## How to:

To download a web page in Gleam, we can use the `curl` module. First, we need to add it to our project dependencies:

```Gleam
  gleam_deps = [
    "lumihq/curl 0.8.0"
  ]
```

Next, we can use the `curl` module's `get` function to specify the URL of the webpage we want to download. We can then use the `body` function to retrieve the content as a string and print it to the console.

```Gleam
import curl

let url = "https://example.com"

test "Downloading a webpage" {
  let result = curl.get(url)
  let content = result.body()
  assert.equals(content, "This is the webpage content!")
}
```

## Deep Dive:

Downloading web pages has long been an essential task in programming, used for a variety of purposes such as data extraction, API integration, and web scraping. Historically, libraries like Python's `urllib` were popular for this task, but newer languages like Gleam offer more efficient and reliable methods.

Alternatives to using the `curl` module include using a web scraping tool like Selenium or using an HTTP client library such as `reqwest` for more complex web interactions.

The `curl` module in Gleam is a wrapper around the cURL command-line tool, providing an ergonomic and safe API for downloading web pages without the need for external programs or dependencies.

## See Also:

- [Gleam Documentation on `curl`](https://gleam.run/modules/lumihq/curl/latest/)
- [Comparison of cURL and `reqwest`](https://www.tecmint.com/curl-vs-wget-vs-httrack-best-linux-download-managers/)