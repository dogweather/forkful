---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-01-20T18:02:36.051914-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

In Ruby, sending an HTTP request with basic authentication involves adding a username and password to your request header. Programmers do this to access resources that require user verification.

## How to:

To send an HTTP request with basic authentication, you'll typically use the `Net::HTTP` module in Ruby. Here's a quick example:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
username = 'your_username'
password = 'your_password'

request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

response = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(request)
}

puts response.body
```

If you run this code with valid credentials, you'll see the body of the response printed out. If the credentials are invalid, you'll get an error message.

## Deep Dive

Basic authentication has a long history as part of web protocols, going back to the early RFCs that defined the workings of the internet. It's a simple method of access control: the username and password are encoded with Base64 and passed in the HTTP header.

However, basic authentication transmits credentials in plaintext (albeit encoded), so it's not secure over HTTP. It's better to use HTTPS to keep credentials safe from prying eyes. 

There are more secure alternatives like OAuth, which is often used for API authentication. OAuth allows users to authorize third-party access without sharing credentials. Still, basic authentication remains in use, especially for internal applications and quick-and-dirty scripting.

One detail to note is that Ruby's `Net::HTTP` doesn’t handle Basic Auth natively until you explicitly use the `basic_auth` method. It's also crucial to handle possible exceptions and error responses that might result from the HTTP request.

## See Also

- Ruby standard library `Net::HTTP` documentation: https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- An intro to OAuth for authentication: https://oauth.net/2/
- More on Ruby and HTTP requests: https://www.rubyguides.com/2019/08/ruby-http-request/