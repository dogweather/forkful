---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a method by which a program or software communicates with a web server, requesting access to specific data or services. As programmers, we do it because it's key to integrating external services, accessing data, or just interacting with the digital world more generally.

## How to:

Here's how you can send a GET request using Net::HTTP in Ruby:

```Ruby
require 'net/http'

uri = URI('http://example.com/index.html')
res = Net::HTTP.get_response(uri)

puts res.body if res.is_a?(Net::HTTPSuccess)
```

When you run it, it sends a GET request to `http://example.com/index.html` and if the response is successful, it prints the response body to the console. Remember to replace `http://example.com/index.html` with your desired URL.

## Deep Dive

Sending HTTP requests is an integral part of programming and web-development, tracing its origins back to the inception of the HTTP protocol in 1991. In Ruby, beyond the built-in Net::HTTP library, alternative libraries like `httparty` and `rest-client` are available which provide a more user-friendly interface.

While sending a request is simple, some aspects take getting used to. For instance, knowing the ins and outs of HTTP request methods - GET, POST, PUT, DELETE, etc., each suitable for different scenarios, is important. Also, the need to properly handle responses can be complex, considering the variety of possible HTTP response status codes and their implications.

## See Also:

For a deep dive into the HTTP protocol, [MDN's guide](https://developer.mozilla.org/en-US/docs/Web/HTTP) is a great start. For a well-rounded understanding of Ruby's Net::HTTP library, the [official Ruby documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/net/http/rdoc/Net/HTTP.html) is comprehensive. Check out the gem pages for [httparty](https://rubygems.org/gems/httparty) and [rest-client](https://rubygems.org/gems/rest-client) for more comfortable alternatives.