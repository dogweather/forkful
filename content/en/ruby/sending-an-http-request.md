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

## Why

Have you ever wanted to interact with a website or web service through your code? You can do so by sending an HTTP request. Whether you want to retrieve data, submit information, or perform other actions, sending an HTTP request allows you to communicate with websites and web services in a simple and efficient way.

## How To

To send an HTTP request in Ruby, you will need to use the `Net::HTTP` library. First, you need to require the library in your code:

```ruby
require 'net/http'
```

Next, you will need to create a `Net::HTTP` object and specify the URL of the website or web service you want to communicate with:

```ruby
uri = URI('https://example.com') # replace with your desired URL
http = Net::HTTP.new(uri.host, uri.port)
```

You can also specify any additional parameters or headers in the `uri` object, such as authentication credentials or specific query parameters.

Once you have set up your `Net::HTTP` object, you can use it to make various types of requests, including GET, POST, PUT, and DELETE. For example, to make a GET request and retrieve data from the specified URL, you can use the `get` method and assign the response to a variable:

```ruby
response = http.get(uri)
```

You can then access the response body, headers, and other information by calling specific methods on the `response` object. Here's an example of printing the body of the response:

```ruby
puts response.body
```

And that's it! You have successfully sent an HTTP request using Ruby.

## Deep Dive

Sending an HTTP request involves several steps under the hood. When you call the `get` method, for example, Ruby sends an HTTP GET request to the specified URL using the TCP protocol. The server then responds with an HTTP response, which contains a status code, headers, and a body.

If the response indicates a successful request (e.g. a status code of 200), you can access the information in the response body to retrieve the data you were looking for. Otherwise, you may need to handle any errors or unexpected responses.

It's also worth noting that the `Net::HTTP` library offers more advanced options, such as persistent connections and SSL/TLS support. You can explore these features further in the official [documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html).

## See Also

- Official `Net::HTTP` documentation: https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html
- HTTP requests tutorial: https://www.theodinproject.com/courses/ruby-programming/lessons/http-networking
- HTTP basics explained: https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html