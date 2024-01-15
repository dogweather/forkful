---
title:                "Sending an http request with basic authentication"
html_title:           "Ruby recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests with basic authentication is a common practice when interacting with APIs. This type of authentication allows users to securely access and retrieve data from a remote server.

## How To

Sending an HTTP request with basic authentication is fairly straightforward in Ruby. Here's an example of how to do it using the `Net::HTTP` library:

```Ruby
require 'net/http'

uri = URI("https://example.com/api")
username = "my_username"
password = "my_password"

request = Net::HTTP::Post.new(uri)
request.basic_auth(username, password)

response = Net::HTTP.start(uri.host, uri.port, use_ssl: true) do |http|
  http.request(request)
end

puts response.code # Outputs HTTP status code, e.g. 200
puts response.body # Outputs response body, e.g. JSON data
```

In this example, we first require the `net/http` library, which allows us to make HTTP requests. Next, we define the URL we want to send the request to and our username and password for basic authentication.

Then, we create a `Net::HTTP::Post` object and pass in our URI. We then use the `basic_auth` method to provide our credentials. Finally, we use `Net::HTTP` to start the connection and make the request, storing the response in a variable.

## Deep Dive

Basic authentication works by requiring a username and password to be sent with every request. This is typically done by encoding the credentials in a special header called "Authorization". The server then decodes the header to verify the user's identity before allowing access to the requested resource.

However, it's important to note that basic authentication is not considered to be the most secure method of authentication. This is because the credentials are sent in plain text, making it easier for them to be intercepted by malicious actors. It's recommended to use more secure methods, such as OAuth, if possible.

## See Also

Here are some additional resources for learning more about sending HTTP requests with basic authentication in Ruby:

- [Ruby on Rails Guide: HTTP Basic Authentication](https://guides.rubyonrails.org/security.html#http-basic-authentication)
- [Net::HTTP rubydocs](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Basic Access Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)