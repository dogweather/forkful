---
title:                "Ruby recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

As a language used for web development, Ruby offers powerful tools for downloading web pages. With this capability, developers can easily retrieve data from websites for various purposes such as web scraping, data analysis, and automated tasks.

## How To

To download a web page using Ruby, we will be using the `open-uri` library, which allows us to open and read URL links. First, we need to require the library in our code:

```Ruby
require 'open-uri'
```

Next, we can use the `open()` method and pass in the URL of the web page we want to retrieve:

```Ruby
page = open("https://www.example.com")
```

This will return a `Tempfile` object, which contains the raw HTML data of the web page. We can then use the `.read` method to read the data and store it in a variable:

```Ruby
html_data = page.read
```

We can also download other types of data, such as images, using the same method. For example, to download an image from a webpage and save it to our local computer, we can use the `copy_stream` method:

```Ruby
# Create a file to save the image
file = open('image.png', 'wb')

# Download and save the image to our local file
file << open('https://www.example.com/image.png').read
```

## Deep Dive

The `open-uri` library uses the `Net::HTTP` library to retrieve data from URLs. This means that it supports various protocols such as HTTP, HTTPS, and FTP. It also supports redirects, basic authentication, and cookie handling.

Furthermore, we can also pass in options to the `open()` method to customize the behavior of the request, such as setting the timeout period or the user-agent. Refer to the official documentation for a full list of available options and their usage.

It's important to note that downloading web pages using Ruby may not always be legal, and it's crucial to check the website's terms and conditions before using any data for commercial purposes.

## See Also

To learn more about downloading web pages in Ruby, here are some helpful links:

- [Ruby official documentation on open-uri](https://ruby-doc.org/stdlib-2.7.1/libdoc/open-uri/rdoc/OpenURI.html)
- [Net::HTTP documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/index.html) 
- [A Beginner's Guide to Web Scraping with Ruby](https://www.scrapingbee.com/blog/web-scraping-ruby/)
- [Open Source Web Scrapers in Ruby](https://github.com/dunyakirkali/open-source-ruby-web-scrapers#back-to-basics)