---
date: 2024-01-20 18:00:56.412555-07:00
description: "Sending an HTTP request with basic authentication involves attaching\
  \ a username and password to a request for access control. Programmers do it for\
  \ simple\u2026"
lastmod: '2024-02-25T18:49:56.795624-07:00'
model: gpt-4-1106-preview
summary: "Sending an HTTP request with basic authentication involves attaching a username\
  \ and password to a request for access control. Programmers do it for simple\u2026"
title: Sending an HTTP request with basic authentication
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves attaching a username and password to a request for access control. Programmers do it for simple auth schemes to protect resources on the server.

## How to:

Here's a basic example using the `CURL` library in C++. Before diving in, make sure you've got `libcurl` installed.

```C++
#include <iostream>
#include <curl/curl.h>

// Simple callback function to handle data received by curl
static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://yourapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // Perform the request, and check for errors
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            std::cout << readBuffer << std::endl;
        }
        
        // Cleanup
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

You'll see a response from the server printed to the console, assuming no errors occurred.

## Deep Dive

Basic authentication is old-school, dating back to the early days of HTTP. Now, industry preference leans towards more secure methods like OAuth and tokens. Despite this, basic auth remains in use, often for internal or simple systems where heavy security layers are bulky overkill.

Under the hood, your username and password are base64-encoded and tucked into the HTTP header. It's simple yet insecure if not over HTTPS because base64 is easily reversible—making HTTPS a must.

If `libcurl` isn't to your taste, consider alternatives like the `cpp-httplib` library, or you can run with `Boost.Beast` for a more hands-on approach.

## See Also

- [libcurl](https://curl.se/libcurl/)
- [cpp-httplib GitHub repository](https://github.com/yhirose/cpp-httplib)
- [Boost.Beast documentation](https://www.boost.org/doc/libs/master/libs/beast/doc/html/index.html)
