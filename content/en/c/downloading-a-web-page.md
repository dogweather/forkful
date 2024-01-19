---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage means grabbing its data and saving it locally. Programmers do this to analyze the content, extract information or for offline browsing.

## How to:

Here, we'll use `libcurl` in C to handle webpage download. Check libcurl documentation for installation-process.

```C
#include <curl/curl.h>
#include <stdio.h>

size_t write_to_string(void *ptr, size_t size, size_t count, void *stream) {
  ((std::string*)stream)->append((char*)ptr, 0, size*count);
  return size*count;
}

int main(void) {
  CURL *curl;
  CURLcode res;
  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    
    std::string response_string;
    std::string header_string;
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_to_string);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_string);
    curl_easy_setopt(curl, CURLOPT_HEADERDATA, &header_string);

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
      
    curl_easy_cleanup(curl);
  }
  curl_global_cleanup();
  return 0;
}
```

This code initializes `libcurl`, sets the webpage URL, points to `write_to_string` as the function to send data to, performs the transfer, and finally cleans up. 

## Deep Dive:

1. **Historical context**: The official HTTP protocol v1.0, supporting 'GET' method for downloading webpages, was standardized in 1996. Today, we have `libcurl` and similar libraries making HTTP requests easier in several programming languages.
   
2. **Alternatives**: Apart from `libcurl`, other alternatives in C are `WinINet`, `CyaSSL`, `axTLS`. Higher-level languages offer simpler interfaces, like the `requests` library in Python.
   
3. **Implementation details**: `libcurl` is event-driven. It maintains multiple connections in a single thread, avoiding overhead of thread context-switching. It processes the request, calls the write callback function `write_to_string` whenever there's received data, passing the received data as an argument.

## See Also:

* libcurl documentation : https://curl.haxx.se/libcurl/c/
* HTTP 1.0 : https://www.ietf.org/rfc/rfc1945.txt
* Alternatives to libcurl : https://www.slant.co/topics/1081/~best-http-clients-for-c