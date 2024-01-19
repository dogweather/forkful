---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication is a method to access protected resources on web servers using a username and password, encoded in base64. Programmers use this for verifying the identity of users before serving sensitive data, thus improving security.

## How to:
We can leverage the `libcurl` library to facilitate this in C. After installing `libcurl`, here's a simplified code to send an HTTP request with Basic Authentication:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();

  if(curl) {
    curl_easy_setopt(curl, CURLOPT_USERNAME, "your_username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "your_password");
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  
  curl_global_cleanup();

  return 0;
}
```
Replace `"your_username"` and `"your_password"` with your credentials.

`curl_easy_perform(curl)` sends the request. If successful, the protected resource will be printed in the terminal. Otherwise, an error message gets printed, indicating why the request failed.

## Deep Dive
Historically, Basic Authentication existed since the earliest days of the web and is part of the HTTP/1.0 spec under RFC 1945. But it's not very secure on its own as the username and password are encoded in base64, which can be easily decoded. Hence, it's commonly used over HTTPS.

Alternatives to Basic Authentication include Digest Authentication, OAuth, JWT, etc., each with its pros and cons. The choice depends on the specific requirements and constraints.

Technically, Basic Authentication in HTTP headers follows the format: `Authorization: Basic <base64_encoded_credentials>`. The `libcurl` library encodes the username and password in base64 and adds this header automatically when we set the `CURLOPT_USERNAME` and `CURLOPT_PASSWORD` options.

## See Also
[CURL libcurl - HTTP Basic Authentication](https://curl.se/libcurl/c/http-basic.html)  
[Wikipedia - Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)  
[MDN Web Docs - HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)