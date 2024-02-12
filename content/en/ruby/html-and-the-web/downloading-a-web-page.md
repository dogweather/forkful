---
title:                "Downloading a web page"
aliases:
- /en/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:45:05.202346-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page means grabbing the HTML content from the internet. Programmers do it to parse the data, scrape information, or monitor changes programmatically.

## How to:
Ruby makes downloading a web page straightforward with libraries like `net/http` and gems like `open-uri`. Here's how to do it using `net/http`:

```Ruby
require 'net/http'
require 'uri'

url = URI.parse('http://example.com') 
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

You'll get the HTML content of `http://example.com` printed out.

Using `open-uri` is even simpler:

```Ruby
require 'open-uri'

downloaded_page = URI.open('http://example.com').read
puts downloaded_page
```

Again, the web page content is displayed on your terminal.

## Deep Dive
Back in the early days of the web, downloading a page was a bit more labor-intensive, involving manual HTTP request crafting. Today, Ruby abstracts much of that complexity away.

Alternatives to `net/http` and `open-uri` include higher-level gems like `HTTParty` and `RestClient`. They offer more features and an object-oriented approach. For heavy-duty web scraping, many Rubyists turn to `Nokogiri` to parse HTML or `Mechanize` which acts like a web browser.

When it comes to implementation, keep in mind that `open-uri` is a wrapper for `net/http`, so it's pretty convenient but may lack some low-level control. `net/http` gives you more control over the request but can be verbose for simple tasks.

## See Also
For further reading and additional resources, check out:

- Ruby's Net::HTTP doc: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Open-URI doc: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- Nokogiri's webpage: [https://nokogiri.org/](https://nokogiri.org/)
- Mechanize gem repository: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- HTTParty gem on GitHub: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- RestClient gem: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
