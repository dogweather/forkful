---
title:                "Sending an http request"
html_title:           "Ruby recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is a way for a program to interact with a web server. This involves sending a message to the server with a specific URL and receiving a response back. Programmers use this functionality to retrieve data or make changes to a website or web application.

## How to:
There are multiple ways to send an HTTP request in Ruby. One simple way is to use the [Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html) module, which is built-in to the standard library. Here's an example of how to use Net::HTTP to make an HTTP GET request and print out the response body:

```Ruby
require 'net/http'

url = URI("https://example.com")

response = Net::HTTP.get_response(url)
puts response.body
```

This code will send a GET request to "example.com" and output the HTML body of the website. You can also use other HTTP methods like POST, PUT, and DELETE with Net::HTTP.

You can also use [HTTParty](https://github.com/jnunemaker/httparty) gem, which provides a more user-friendly interface for making HTTP requests. Here's an example of using HTTParty to make a POST request and print out the response:

```Ruby
require 'httparty'

response = HTTParty.post("https://jsonplaceholder.typicode.com/posts",
                         body: { title: "New Post", body: "This is a new post." })
puts response.body
```

This code will make a POST request to the "jsonplaceholder" API and print out the response, which in this case will be the newly created post.

## Deep Dive
Sending HTTP requests has been a core feature of programming since the early days of the web. Before the modern tools and libraries we have today, programmers had to manually set up sockets and send raw HTTP requests. The introduction of libraries like Net::HTTP and HTTParty have made it much easier to interact with web servers.

There are also alternative ways to make HTTP requests in Ruby, such as [Faraday](https://github.com/lostisland/faraday) and [RestClient](https://github.com/rest-client/rest-client). These libraries provide more advanced features like request logging, authentication, and error handling.

Behind the scenes, sending an HTTP request involves establishing a TCP connection with the web server, sending the request headers and body, and then receiving and processing the response.

## See Also
- [Net::HTTP documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty documentation](https://github.com/jnunemaker/httparty)
- [Faraday documentation](https://github.com/lostisland/faraday)
- [RestClient documentation](https://github.com/rest-client/rest-client)