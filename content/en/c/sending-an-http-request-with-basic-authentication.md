---
title:                "Sending an http request with basic authentication"
html_title:           "C recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending an HTTP request with basic authentication is a common practice in web development and can add an extra layer of security to your application. It allows users to enter a username and password to access restricted areas or perform certain actions.

## How To
To send an HTTP request with basic authentication in C, you will need to first initialize the necessary libraries. Then, you can use the `curl_easy_setopt` function to set the username and password for the request. Finally, use the `curl_easy_perform` function to send the request and handle the response.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  // initialize curl
  curl = curl_easy_init();
  if (curl) {
    // set the URL
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");

    // set the username and password
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

    // perform the request
    res = curl_easy_perform(curl);
    if (res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    // cleanup
    curl_easy_cleanup(curl);
  }

  return 0;
}
```
**Output:**
```
HTTP/1.1 200 OK
Date: Thu, 01 Jul 2021 00:00:00 GMT
Server: Apache
Content-Length: 234
Content-Type: text/html
```

## Deep Dive
In this example, we used the `curl_easy_setopt` function to set the options for our request. The `CURLOPT_USERNAME` and `CURLOPT_PASSWORD` options allow us to provide the basic authentication credentials. It is important to note that these options will only work for HTTP requests and not HTTPS requests. In those cases, other methods such as setting the `Authorization` header may be necessary. Additionally, it is always recommended to use a secure method of storing and retrieving the username and password, such as using environment variables.

## See Also
- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [libcurl - Using libcurl with HTTP basic authentication](https://curl.se/libcurl/c/libcurl-tutorial.html#HTTPBASICAUTH)
- [HTTP Authentication: Basic and Digest Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)