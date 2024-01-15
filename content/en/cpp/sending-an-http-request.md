---
title:                "Sending an http request"
html_title:           "C++ recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
Sending HTTP requests is an important aspect of web development and programming in general. It allows for communication between a client and a server, making it possible to retrieve and send information over the internet.

## How To
Sending an HTTP request in C++ is fairly straightforward. The following code snippet shows a basic example of how to send a GET request using the C++ networking library:

```C++
#include <iostream>
#include <boost/asio.hpp>

using namespace boost::asio; // For simplicity, not recommended for large projects

int main() {
  // Create an io_context object to handle asynchronous operations
  io_context ioc{};
  
  // Create a TCP resolver to resolve a given endpoint
  tcp::resolver resolver{ioc};
  
  // Resolve the endpoint of the server we want to send a request to
  auto endpoints = resolver.resolve("www.example.com", "http");
  
  // Create a TCP socket and connect it to the resolved endpoint
  tcp::socket socket{ioc};
  boost::asio::connect(socket, endpoints);
  
  // Create a GET request message
  std::string request = "GET / HTTP/1.1\r\n"
                        "Host: www.example.com\r\n"
                        "Connection: close\r\n\r\n";
                        
  // Send the request to the server
  write(socket, buffer(request));
  
  // Read the server's response
  std::string response;
  std::array<char, 2048> buffer;
  while(read(socket, buffer, boost::asio::transfer_at_least(1))) {
    response.append(buffer.data(), buffer.data() + buffer.size());
  }
  
  // Print out the response
  std::cout << response << std::endl;
  
  return 0;
}
```

The output of this code would be the HTML for the home page of example.com. You can modify the request message to send different HTTP methods and headers based on the server's requirements.

## Deep Dive
To send an HTTP request, we first need to understand the structure of a request message. It consists of three parts: the request line, request headers, and a body (optional). The request line contains information about the method, path, and version of HTTP being used. Request headers convey additional information about the request, such as the host, connection type, and accepted content types. And finally, the body carries any data that needs to be sent with the request. The code example above shows how we can construct a basic request message using these parts.

Apart from the basic TCP socket and networking libraries, other C++ libraries can be used to send HTTP requests. Some popular options include the Poco C++ libraries, the cItppNetLib library, and the libcurl library. These libraries offer more high-level functions and abstractions for sending HTTP requests, making the process even easier.

## See Also
- [Boost C++ Libraries](https://www.boost.org/)
- [Poco C++ Libraries](https://pocoproject.org/)
- [cItppNetLib Library](https://citpp.it-innovation.soton.ac.uk/)
- [libcurl Library](https://curl.se/libcurl/)