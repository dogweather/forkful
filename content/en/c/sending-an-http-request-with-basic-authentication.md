---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-01-20T18:01:03.575009-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication involves adding a header with a username and password to access protected resources. Programmers do it to interact with web services that require login credentials for operation.

## How to:
To send an HTTP request with basic authentication in C, you'd typically use a library like libcurl. Here's a short example:

```c
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        // Set the URL that is about to receive our POST request
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        
        // Set the basic authentication credentials
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        
        // Perform the request 
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        
        // Clean up
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

Output will depend on the response from the server.

## Deep Dive
Sending an HTTP request with basic authentication is quite an old way to control access to web resources. Designed in the early days of the internet, it's not the most secure method because the credentials are encoded with base64, not encrypted.

Alternatives like OAuth and API keys are now recommended for better security. However, basic auth is still useful for simple scripts or internal tools where these risks are acceptable.

Implementation is usually done with libraries like libcurl or custom socket programming if you need more control. Basic auth headers can be manually constructed, but it's cumbersome and error-prone, so libraries are the way to go.

## See Also
- cURL library documentation: https://curl.haxx.se/libcurl/c/
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- HTTP authentication MDN web docs: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Introduction to OAuth: https://oauth.net/
