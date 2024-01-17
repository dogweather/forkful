---
title:                "Downloading a web page"
html_title:           "Ruby recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page is the process of retrieving data from a specific URL, usually in the form of HTML, and saving it locally. This allows programmers to access and manipulate website data for various purposes such as data analysis, web scraping, and creating web applications that consume APIs.

## How to:

To download a web page in Ruby, we can use the standard library's `OpenURI` module. Here's a simple example:

```ruby
require 'open-uri'

url = "https://en.wikipedia.org/wiki/Ruby_(programming_language)"
html = open(url).read

puts html
```

This code will retrieve the HTML data from the given URL and save it to a variable named `html`. We can then manipulate this data as needed. For example, we can use regular expressions to extract specific information from the webpage.

## Deep Dive

The `OpenURI` module is a standard library in Ruby that provides a simple interface for retrieving and reading data from URLs. It supports various protocols including HTTP, HTTPS, and FTP. Prior to Ruby 1.9, the `open-uri` library had to be explicitly required, but it is now included by default.

There are also alternative frameworks for downloading web pages in Ruby, such as `HTTParty` and `Faraday`, which offer more advanced features and options. Additionally, there are third-party gems that focus specifically on web scraping, such as `Nokogiri` and `Mechanize`.

Behind the scenes, the `OpenURI` module uses the `net/http` module to make HTTP requests and retrieve data from websites. It also handles redirects and other HTTP responses gracefully.

## See Also

- [OpenURI documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)
- [HTTParty gem](https://github.com/jnunemaker/httparty)
- [Faraday gem](https://github.com/lostisland/faraday)
- [Nokogiri gem](https://github.com/sparklemotion/nokogiri)
- [Mechanize gem](https://github.com/sparklemotion/mechanize)