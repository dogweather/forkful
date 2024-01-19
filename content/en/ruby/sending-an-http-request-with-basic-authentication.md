---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Sending HTTP Requests with Basic Authentication in Ruby

## What & Why?
Sending an HTTP request with basic authentication is about getting data from a server that needs a username and password. It's used when you're dealing with protected information that you want only specific individuals to access.

## How to:
Ruby's `net/http` library makes sending HTTP requests and handling responses relatively straightforward.

Here's an example:

```Ruby
require 'net/http'
require 'uri'
require 'base64'

uri = URI('http://example.com')

http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth 'username', 'password'

response = http.request(request)

puts response.body
```

In this script, we're requiring necessary libraries, preparing an HTTP request with basic authentication, sending the request, and printing out the server response.

## Deep Dive
Back in the day, basic authentication seemed perfect for a world where simplicity was king. Yet, basic authentication isn't secure on its own — your credentials are essentially in plaintext. For this reason, it's recommended you use it over HTTPS, not HTTP.

Alternatives to basic authentication include token-based methods, like OAuth, JWT, etc. These methods do not require passing credentials in every request which adds an extra level of security.

In our Ruby example, we used a built-in library, `net/http`. Though it’s a bit verbose, `net/http` is part of Ruby’s standard library, doesn't add dependencies, and is reliable.

Want something less verbose? Check out the Faraday and HTTParty gems. They abstract away some complexities of `net/http`, offering a simplified API.

## See Also
- An in-depth guide on HTTP authentication: [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication).
- Details about Ruby’s net/http library: [Ruby Doc - Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html).
- Ruby Authentication Libraries: [Devise](https://github.com/heartcombo/devise), [Authlogic](https://github.com/binarylogic/authlogic), [Clearance](https://github.com/thoughtbot/clearance).
- Alternatives to `net/http`: [Faraday](https://github.com/lostisland/faraday), [HTTParty](https://github.com/jnunemaker/httparty).