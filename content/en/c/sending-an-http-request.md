---
title:                "Sending an http request"
html_title:           "C recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for a program to communicate with a server over the internet. This is commonly used in web development to retrieve data or trigger specific actions on a server. Programmers use HTTP requests to access and manipulate data from remote servers, making it a vital tool for creating dynamic web applications.

## How to:

To send an HTTP request in C, we first need to include the `curl.h` library. This will provide us with the necessary functions to make HTTP requests. Let's take a look at an example code:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    res = curl_easy_perform(curl); 
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

In this example, we are using the `curl_easy_init()` function to initialize a new curl session. Then, we use the `curl_easy_setopt()` function to set the URL of the server we want to make the request to. Finally, we use `curl_easy_perform()` to execute the request. The result of the request will be stored in the `res` variable. 

## Deep Dive

Sending HTTP requests in C has been made possible by the `libcurl` library. This library was first released in 1997 and is still actively maintained. It provides a high-level API for performing various internet protocols, including HTTP. 

Alternatively, there are other ways to send HTTP requests in C, such as using the `libmicrohttpd` library or implementing the HTTP protocol from scratch. However, `libcurl` is the most widely used and recommended option for sending HTTP requests in C.

Behind the scenes, the `libcurl` library uses the `libmbedcrypto` and `libzlib` libraries for encryption and compression, respectively. This ensures the security and efficiency of the HTTP requests.

## See Also

To learn more about sending HTTP requests in C, check out the official `libcurl` documentation: https://curl.se/libcurl/
To explore other alternatives for sending HTTP requests, you can refer to the `libmicrohttpd` documentation: https://www.gnu.org/software/libmicrohttpd/