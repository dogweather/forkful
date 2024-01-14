---
title:                "Ruby recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why 

Sending HTTP requests is a fundamental task in modern web development. It allows us to retrieve data from external APIs, interact with databases, and communicate with web servers. In this blog post, we will explore how to send HTTP requests using Ruby and why it's an important skill to have in your programming toolbox.

## How To 

Sending an HTTP request in Ruby is a simple process. We can use the `Net::HTTP` library to make requests to a specific URL. Let's take a look at a basic example:

```ruby
require 'net/http'

url = URI("https://example.com/api/users")

response = Net::HTTP.get_response(url)
puts response.body
```

In this example, we first require the `net/http` library, which is included by default in Ruby. Then, we specify the URL that we want to make a request to using the `URI` class. Next, we use the `Net::HTTP.get_response` method to send a `GET` request to the specified URL. This will return a response object, which we can access the body of using `response.body`. In this case, the output will be the HTML of the example.com homepage.

We can also send other types of requests, such as `POST` and `PUT`, by using the corresponding methods in the `Net::HTTP` class. For example:

```ruby
require 'net/http'

url = URI("https://example.com/api/users")

# create a new HTTP request
req = Net::HTTP::Post.new(url)

# add parameters to the request
req.set_form_data({'username' => 'john', 'password' => '123456'})

# send the request and print the response status code
res = Net::HTTP.start(url.hostname, url.port, use_ssl: true) do |http|
  http.request(req)
end

puts res.code # 200, indicating success
```

In this example, we use the `Net::HTTP::Post` class to create a new `POST` request to the specified URL. Then, we add our desired parameters to the request using the `set_form_data` method. Finally, we send the request using the `Net::HTTP.start` method and print the response status code, which should be `200` indicating that the request was successful.

## Deep Dive 

Sending an HTTP request involves several steps behind the scenes. Let's take a closer look at the `Net::HTTP` class and how it handles requests.

The first step is to create a `Net::HTTP` object using the `Net::HTTP.new` method. This object represents the connection to the server. Then, we use the `start` method to establish the connection and send the request. The `start` method also has the option to use SSL for secure connections.

When sending the request, the `req` object in our `POST` example is used to store the request type, headers, and body. The request is then sent to the server, and the response is stored in the `res` object.

There are also other methods and classes in the `Net::HTTP` library that allow us to customize our requests. For example, we can add custom headers, set timeouts, and handle redirects.

## See Also 

For more information on sending HTTP requests in Ruby, check out the following resources:

- [Official Net::HTTP documentation](https://ruby-doc.org/stdlib-1.9.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [Ruby Guides tutorial on sending HTTP requests](https://www.rubyguides.com/2018/08/ruby-http-request/)
- [HTTParty gem for making HTTP requests in Ruby](https://github.com/jnunemaker/httparty)