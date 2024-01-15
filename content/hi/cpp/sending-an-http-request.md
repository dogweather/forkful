---
title:                "एक http अनुरोध भेजना"
html_title:           "C++: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Kyun

HTTP request bhejna aasaan tareeke se server se data ko prapt karne ka sabse prabhavshali aur prachalit tarika hai. Ye web development, data exchange aur online communication ko sambhav banata hai.

## Kaise Kare

Aap HTTP protocol ka istemal karke ek request ko bhej sakte hain. Iske liye, aapko ek URL, request method (GET, POST, PUT, DELETE, etc.) aur optional headers aur body data ki zaroorat hoti hai.

```C++
// Example request code using the cURL library
#include <curl/curl.h>

int main() {
    // Initialize cURL session
    CURL *curl = curl_easy_init();

    // Set request URL
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");

    // Set request method (GET, POST, etc.)
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");

    // Optional headers
    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

    // Optional body data
    // curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "body=data");

    // Perform the request
    CURLcode res = curl_easy_perform(curl);

    // Check for errors
    if (res != CURLE_OK) {
        fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
    }

    // Cleanup
    curl_easy_cleanup(curl);

    return 0;
}
```
### Output

Is code ko execute karne par, aap server se request ko bhejenge aur uska response ko console par dekh sakte hain. Agar koi error aata hai toh uska message dikhega.

## Gehri Jhanjhana

HTTP request bhejne ke liye, internetworking protocols aur internet ke sahi se samajh ki zaroorat hai. HTTP request me header fields, status codes, mime types jaise concepts hote hain jo aapko samajhna zaroori hai. Iske alawa, aapko request body ka bhi dhyaan rakhna padega, jiske liye JSON, XML, ya binary data jaise formats ka istemal kiya ja sakta hai.

See Also

- [HTTP Protocol Explained](https://www.freecodecamp.org/news/http-and-everything-you-need-to-know-about-it/)
- [cURL Documentation](https://curl.haxx.se/docs/)