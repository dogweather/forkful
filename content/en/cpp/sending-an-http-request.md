---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Sending HTTP Requests in C++

## What & Why?
Sending an HTTP request involves a client (your application) asking a server for specific resources. Programmers do this to communicate with web servers, APIs, and other web-based services.

## How to:
C++ doesn't offer in-built support for HTTP requests, but we can use popular libraries, like cURL or Boost Beast. Let's proceed with cURL for simplicity. First, install the library if you haven't yet.

```C++
sudo apt-get install libcurl4-openssl-dev
```

Here's a basic example of how to send an HTTP GET request:

```C++
#include <iostream>
#include <curl/curl.h>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main()
{
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if (curl) {
      curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
      res = curl_easy_perform(curl);
      curl_easy_cleanup(curl);

      std::cout << readBuffer << std::endl;  
    }
    return 0;
}
```

Output will be HTML from example.com.

## Deep Dive
C++, unlike Python or JavaScript, doesn't have built-in HTTP request functions. Early C++ use was low-level system tasks, not web-based work.

Alternatives to cURL are Boost Beast and POCO, offering more comprehensive libraries.

cURL works by setting up an easy handle, attaching options (url, method, etc.), and executing. It's critical to cleanup afterwards, releasing system resources.

## See Also
[cURL library](https://curl.haxx.se/libcurl/)  
[Boost Beast](https://www.boost.org/doc/libs/1_75_0/libs/beast/doc/html/index.html)  
[POCO](https://pocoproject.org/)