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

## What & Why?
Sending an HTTP request with basic authentication is a way for programmers to securely access information from a web server. It involves adding a username and password to the request, which is then verified by the server before granting access. Programmers do this to ensure that sensitive information is only accessible to authorized users.

## How to:
To send an HTTP request with basic authentication in Ruby, we can use the Net::HTTP library. First, we need to require the library:
```Ruby
require 'net/http'
```
Next, we create a new instance of the Net::HTTP class with the URL of the server we want to access:
```Ruby
http = Net::HTTP.new("www.example.com")
```
Then, we create a new request object with the HTTP method we want to use (e.g. GET, POST) and the endpoint we want to access:
```Ruby
request = Net::HTTP::Get.new("/api/users")
```
Next, we add the username and password to the request using the Net::HTTP::BasicAuth class:
```Ruby
request.basic_auth("username", "password")
```
Finally, we can send the request and receive a response from the server:
```Ruby
response = http.request(request)
```
The response object contains information such as the status code and body of the response, which we can access using methods like `response.code` and `response.body`.

## Deep Dive
HTTP basic authentication has been around since the early days of the internet and is a widely used method for securing web requests. It is a simple and elegant solution, but it has some limitations, such as lack of encryption for the username and password, making it vulnerable to attacks like man-in-the-middle.

Some alternatives to basic authentication include Digest authentication, which includes a nonce value to prevent replay attacks, and OAuth, which allows for more granular access control and does not require sharing of passwords.

In Ruby, basic authentication is implemented using the Authorization header, which includes the username and password encoded in Base64. It is important to note that this encoding does not provide any encryption and can be easily decoded, so it is recommended to use HTTPS when sending requests with basic authentication.

## See Also
- [Net::HTTP documentation](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/)
- [HTTP basic authentication RFC](https://tools.ietf.org/html/rfc2617)
- [Alternatives to HTTP basic authentication](https://auth0.com/blog/alternatives-to-basic-authentication-with-apache-and-nginx/)