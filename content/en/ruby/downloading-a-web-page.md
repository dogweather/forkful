---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means acquiring it from the internet onto your local setup. Programmers do this to access the page's data, scrape information, or test website functionality. 

## How to:

Here's simple code to download a webpage in Ruby using `open-uri` and `nokogiri` gems.

```Ruby
require 'open-uri'
require 'nokogiri'

def download_web_page(url)
  Nokogiri::HTML(URI.open(url))
end

page = download_web_page('https://www.example.com')

puts page.title
```

The output would be the title of the website `https://www.example.com`.
   
## Deep Dive

Originally, folks used telnet applications to retrieve web pages, but it got easier with the World Wide Web's inception. Ruby's `open-uri` and `nokogiri` simplify this task, taking care of HTTP interaction and HTML parsing. 

An alternative means is using the `Net::HTTP` standard library. It's a bit more intricate but offers finer control over HTTP requests. 

```Ruby
require 'net/http'
require 'uri'

def download_web_page(url)
  uri = URI.parse(url)
  response = Net::HTTP.get_response(uri)
  
  response.body
end

page = download_web_page('https://www.example.com')

puts page
```

This code fetches the entire HTML content of the web page.

## See Also:

Check out these for more knowledge:

- Ruby Doc's URI::open: [`https://ruby-doc.org/stdlib-3.0.2/libdoc/open-uri/rdoc/URI.html#method-c-open`](https://ruby-doc.org/stdlib-3.0.2/libdoc/open-uri/rdoc/URI.html#method-c-open)
  
- Nokogiri at [`https://nokogiri.org/`](https://nokogiri.org/)

- Net::HTTP in Ruby Docs: [`https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html`](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)