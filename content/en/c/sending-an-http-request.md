---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a method used to interact with resources at a specified HTTP endpoint, typically a web server. Programmers do it as a means to fetch, send or update data over the internet.

## How to:

Here's a simple implementation in C of sending an HTTP GET request using the popular networking library libcurl:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  curl_global_cleanup();

  return 0;
}
```

This will simply initiate an HTTP GET request to example.com. To build and run this C program, make sure you have libcurl installed, then you can use gcc (or your preferred C compiler):

```bash
gcc main.c -lcurl -o main 
./main
```

## Deep Dive

The historical basis for HTTP requests goes back to the initiation of the HTTP protocol itself, around 1989 by Tim Berners-Lee at CERN. The fundamental structure of HTTP requests have remained largely unchanged since, with more functionality added via headers and other HTTP methods.

Alternatives to directly sending HTTP requests (like we did in the above C code) can be to use higher-level libraries like the RestClient-C library (which still uses libcurl under the hood), or to use other languages that might have more comprehensive standard libraries for HTTP communication (like Python with its 'requests' library).

Implementation specifics of sending HTTP requests can greatly vary, and it's largely dependent on the requirements of your task. For instance, sending an HTTP POST request will require you to set additional CURL options (like `CURLOPT_POSTFIELDS`) compared to a simple GET request. If you're dealing with HTTPS requests, you'll want to look into how to properly set up SSL/TLS with libcurl.

## See Also

For a detailed overview of the HTTP protocol: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview

For libcurl programming: https://curl.se/libcurl/c/

For RestClient-C: https://github.com/mrtazz/restclient-c

You can also check out the 'requests' library if you plan on using Python: https://docs.python-requests.org/en/master/.