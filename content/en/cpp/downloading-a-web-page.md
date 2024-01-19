---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Article Title: Downloading a Web Page in C++

## What & Why?

Downloading a web page is the act of pulling down HTML content from a specific URL to work with it locally. Programmers often do this to parse data, automate actions or test functionality. 

## How to:

Modern C++ has a powerful library complement for network operations: `Boost.Beast` for HTTP(S) and `Boost.Asio` for lower level TCP/UDP. However, for now, we'll use libcurl because it's easy and widely adaptable. Make sure you've got libcurl installed. Here's a simple snippet to download a webpage's HTML.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

std::size_t callback(
    const char* in,
    std::size_t size,
    std::size_t num,
    std::string* out)
{
    const std::size_t totalBytes(size * num);
    out->append(in, totalBytes);
    return totalBytes;
}

int main()
{
    CURL* curl = curl_easy_init();

    // Set remote URL.
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");

    // Don't bother trying IPv6, which would increase DNS resolution time.
    curl_easy_setopt(curl, CURLOPT_IPRESOLVE, CURL_IPRESOLVE_V4);

    // Don't wait forever, time out after 10 seconds.
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10);

    // Follow HTTP redirects if necessary.
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    // Response information.
    int httpCode(0);
    std::unique_ptr<std::string> httpData(new std::string());

    // Hook up data handling function.
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);

    // Hook up data container (will be passed as the last parameter to the
    // callback handling function).  Can be any pointer type, since it will
    // internally be passed as a void pointer.
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, httpData.get());

    // Run our HTTP GET command, capture the HTTP response code, and clean up.
    curl_easy_perform(curl);
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &httpCode);
    curl_easy_cleanup(curl);

    if(httpCode == 200)
    {
        std::cout << "\nGot successful response from " << url << " :\n";
        std::cout << httpData->c_str() << "\n";
    }
    else
    {
        std::cout << "Couldn't GET from "<< url <<" - exiting\n";
    }

    return 0;
}
```

## Deep Dive

Downloading webpages has been around since the formative days of the internet. Early HTTP was simple and unencrypted, evolving over time to include secure transport (HTTPS), better status codes, and full-featured libraries.

Alternatives to libcurl include Boost.Beast and others. If you need low-level networking or websocket support, opt for Boost.Beast tied with Boost.Asio.

The C++ code leverages HTTP's simple request-response cycle. We make a GET request to the server hosting our desired page. The server responds with the page's HTML, which our program processes, here just printing it to stdout.

## See Also

1. libcurl - [https://curl.haxx.se/libcurl/c/](https://curl.haxx.se/libcurl/c/)
2. HTTP - [https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
3. Boost.Beast - [https://www.boost.org/doc/libs/develop/libs/beast/](https://www.boost.org/doc/libs/develop/libs/beast/)