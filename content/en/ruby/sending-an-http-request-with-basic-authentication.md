---
title:                "Ruby recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why 

Sending HTTP requests with basic authentication is an essential skill for any web developer using Ruby. This allows for secure communication between a client and server, ensuring that sensitive information is only accessed by authorized users.

## How To

To send an HTTP request with basic authentication using Ruby, follow these simple steps:

1. Require the 'net/http' library in your code:
   
   ```ruby
   require 'net/http'
   ```

2. Create a URI object for the endpoint you wish to send the request to: 
   
   ```ruby
   uri = URI('https://example.com/api')
   ```

3. Create a new Net::HTTP object with the URI as the argument:
   
   ```ruby
   http = Net::HTTP.new(uri.host, uri.port)
   ```

4. Use the Net::HTTP::Get method to create a GET request with the URI path:
   
   ```ruby
   request = Net::HTTP::Get.new(uri.path)
   ```

5. Use the Net::HTTP::BasicAuth method to specify the username and password for the basic authentication:
   
   ```ruby
   request.basic_auth 'username', 'password'
   ```

6. Send the request using the Net::HTTP::start method and store the response in a variable: 
   
   ```ruby
   response = http.start do |http|
     http.request(request)
   end
   ```

7. You can then access the response code, message, and body using the response object: 
   
   ```ruby
   puts "Response Code: #{response.code}"
   puts "Response Message: #{response.message}"
   puts "Response Body: #{response.body}"
   ```

## Deep Dive

While using basic authentication for HTTP requests may seem straightforward, there are a few things to keep in mind. Firstly, ensure that the URI you are sending the request to is an HTTPS endpoint, as basic authentication can easily be intercepted over HTTP.

Additionally, basic authentication should only be used for internal APIs or when securely communicating within the same network. For external APIs, it is recommended to use OAuth or API keys as they provide a more robust and secure form of authentication.

Using the code example provided in the "How To" section, you can also add headers to your request using the Net::HTTP::Get method. This allows for more customization and can help with avoiding any rate-limiting restrictions from the server.

Finally, it is good practice to handle any potential errors or exceptions when sending HTTP requests. This can be done using the begin-rescue-end block in Ruby and properly handling any exceptions that may arise.

## See Also

- Net::HTTP Ruby Documentation: https://ruby-doc.org/stdlib-2.5.0/libdoc/net/http/rdoc/Net/HTTP.html
- Tutorial on Secure HTTP Requests in Ruby: https://blog.appsignal.com/2020/05/20/secure-http-requests-in-ruby.html
- HTTP Basic Authentication vs. OAuth: What You Need to Know: https://www.digitalocean.com/community/tutorials/http-basic-authentication-vs-oauth-what-you-need-to-know