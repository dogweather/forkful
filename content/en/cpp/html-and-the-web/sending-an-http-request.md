---
title:                "Sending an HTTP request"
date:                  2024-01-20T17:59:22.887557-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request fetches data from a web server. Programmers do this to interact with web services, gather info, or communicate between systems.

## How to:

```C++
#include <iostream>
#include <cpr/cpr.h> // Make sure to install the CPR library first

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // Outputs the response body
    return 0;
}
```

Sample output:
```json
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/7.64.1"
  },
  "origin": "0.0.0.0",
  "url": "https://httpbin.org/get"
}
```

## Deep Dive
HTTP requests have been crucial since the advent of the web; they follow a client-server model. Prior to C++ libraries like CPR, sending HTTP requests typically meant using `libcurl` directly, or integrating with another language better equipped for web communication.

CPR, which stands for C++ Requests, is a simple-to-use wrapper inspired by Python's `requests` module. It abstracts away many of `libcurl`'s complexities. Alternatives still exist, like Boost.Beast for lower-level HTTP/S operations, or POCO libraries offering portability.

Diving under the hood, sending an HTTP request involves setting up a TCP connection, formatting a request compliant with the HTTP protocol, then parsing the response. Getting this right from scratch is non-trivial due to error handling, HTTP version complexities, and security considerations.

## See Also

- CPR Github Repository: https://github.com/libcpr/cpr
- `libcurl` documentation for more advanced usage: https://curl.se/libcurl/
- Official Boost.Beast documentation: https://www.boost.org/doc/libs/release/libs/beast/
- POCO C++ Libraries documentation: https://pocoproject.org/docs/
