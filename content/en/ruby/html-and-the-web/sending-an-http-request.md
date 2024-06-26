---
date: 2024-01-20 18:00:18.656983-07:00
description: 'How to: Ruby makes it pretty easy to send HTTP requests. Here''s the
  quickest way with the Net::HTTP standard library.'
lastmod: '2024-03-13T22:45:00.547810-06:00'
model: gpt-4-1106-preview
summary: Ruby makes it pretty easy to send HTTP requests.
title: Sending an HTTP request
weight: 44
---

## How to:
Ruby makes it pretty easy to send HTTP requests. Here's the quickest way with the Net::HTTP standard library.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)
puts response
```

This will output the HTML content of `http://example.com`.

You might want to post data too:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2')
puts res.body
```

This sends a POST request with data and shows the response.

## Deep Dive:
In the past, sending HTTP requests was clunkier, and you may have needed to use a gem like `HTTParty`. But Ruby's built-in `Net::HTTP` library has come a long way. It now supports most things you'll need.

But, `Net::HTTP` can be verbose. If your project needs more HTTP features or syntactic sugar, `HTTParty` or `Faraday` are great alternatives. These gems provide a more expressive API and can handle more complex scenarios like middleware or different adapters.

Fundamentally, sending an HTTP request with Ruby involves creating an HTTP client, setting up a request object with method, headers, and body if needed, then dispatching the request to receive a response.

HTTParty example:

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com')
puts response.body
```

This does the same thing as `Net::HTTP.get` but with less configuration.

## See Also:
For more detailed info, Ruby's docs are super helpful:
- Net::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty: https://github.com/jnunemaker/httparty
- Faraday: https://lostisland.github.io/faraday/

And if you've got a thick appetite for Ruby's HTTP networking, take a gander over at:
- Ruby's Open URI: https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/OpenURI.html
- WebMock for testing HTTP requests: https://github.com/bblimke/webmock
