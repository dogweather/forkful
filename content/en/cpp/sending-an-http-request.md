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

## What & Why?

Sending an HTTP request is the process of making a request to a web server using the HTTP protocol. Programmers do this to retrieve data or information from a server, such as accessing a web page or retrieving data from an API.

## How to:
Sending an HTTP request in C++ can be done using the built-in library called "curl". Here's a simple example of sending a GET request:

```C++
#include <iostream>
#include <curl/curl.h> // include the curl library

int main()
{
    // initialize a CURL object
    CURL *curl;
    // set the URL to send the request to
    curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
    // send the request and print the response
    CURLcode res = curl_easy_perform(curl);
    if (res == CURLE_OK)
    {
        std::cout << "Request sent successfully!";
    }
    else
    {
        std::cout << "Error sending request: " << curl_easy_strerror(res);
    }
    // clean up the CURL object
    curl_easy_cleanup(curl);
    return 0;
}
```

This example uses the "curl_easy_init" function to create a CURL object, sets the URL using the "curl_easy_setopt" function, and then uses "curl_easy_perform" to send the request. This function also returns a CURLcode, which we can check to see if the request was successful or not. Finally, we clean up the CURL object using "curl_easy_cleanup".

## Deep Dive:
In the early days of the internet, the protocol used for retrieving data from a server was called "FTP" (File Transfer Protocol). However, as the need for more sophisticated and complex web applications arose, HTTP (HyperText Transfer Protocol) was developed to allow for more flexible communication between clients and servers. Today, HTTP is the most widely used protocol for retrieving data from web servers.

An alternative to using the "curl" library for sending HTTP requests would be to create a TCP connection and manually send the request using sockets. However, this would require more code and would not be as user-friendly as using a library like "curl". Additionally, there are other HTTP libraries available for C++ such as "libhttp" and "cpp-httplib".

Sending an HTTP request involves creating a connection, sending the request, and receiving the response. The request typically contains a method (such as GET or POST), a URL, and optional request headers and body. The response will also have headers, status code, and a body.

## See Also:
- [libcurl docs](https://curl.haxx.se/libcurl/)
- [cpp-httplib](https://github.com/yhirose/cpp-httplib)
- [libhttp](https://github.com/brainboxdotcc/libhttp)