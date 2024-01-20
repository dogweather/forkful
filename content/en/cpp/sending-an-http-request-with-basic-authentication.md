---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves transmitting credentials (username and password) as part of the header in HTTP requests. Programmers do this to access protected resources or perform tasks on behalf of a user in a secure manner.

## How to:

First, let's get started with the C++ 'cpp-httplib' library. Make sure to acquire the library at 'https://github.com/yhirose/cpp-httplib'. Here's a simple code example:

``` C++
#include <httplib.h>

int main() {
  httplib::Client cli("localhost", 1234);
  
  httplib::Headers headers = {
    {"Authorization", "Basic " + httplib::base64_encode("user:pass")}
  };

  auto res = cli.Get("/resource/path", headers);
  
  if (res && res->status == 200) {
    std::cout << res->body << std::endl;
  } else {
    std::cout << "HTTP request failed." << std::endl;
  }

  return 0;
}
```
This code establishes a connection to a local server using basic authentication. If the request is successful, the server response's body will be printed to the console.

## Deep Dive 

Basic authentication was introduced as part of the HTTP/1.0 protocol in the early days of the World Wide Web. Since then, it has been widely adopted due to its simplicity and ease of implementation.

However, it has a significant security flaw: it sends credentials in plaintext, base64 encoded. As such, it's susceptible to man-in-the-middle attacks and must never be used over unencrypted connections.

For better security, many modern APIs and server-side applications prefer token-based mechanisms such as OAuth or opt for stronger digest authentication methods. In C++, libraries such as 'libcurl' and 'Boost Beast' can be used to implement these alternative strategies.

The 'cpp-httplib' library used in our example is a C++11 single-header HTTP/HTTPS library. It's built on top of BSD-sockets, making it portable across systems and making the implementation lean. 

## See Also 

To delve deeper into the world of secure HTTP requests and C++ networking, check out these resources:

- cpp-httplib Library GitHub: https://github.com/yhirose/cpp-httplib
- C++ Networking: https://en.cppreference.com/w/cpp/io/network
- Boost Beast Library: https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html
- libcurl Library: https://curl.se/libcurl/c/libcurl.html