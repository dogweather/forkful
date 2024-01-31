---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:12.893048-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Sending an HTTP request means asking a server for data or action. Programmers do it to interact with web services, grab information, or send data over the internet.

## How to: (Як це зробити:)

Here's a quick example using libcurl in C to send a simple HTTP GET request.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl = curl_easy_init();
    if(curl) {
        CURLcode res;
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);
        if (res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Expected output: The content of `http://example.com` is printed to stdout, or an error message if the request failed.

## Deep Dive (Поглиблений Розбір)

Initially, HTTP requests were manual telnet sessions. Automation came with libraries like libcurl and languages building in functionality.

Alternatives include sockets programming in C for a more manual approach, or high-level protocols in other languages like Python's `requests`.

Details: Libcurl is versatile - supports GET, POST, and more. Under the hood, it handles connection persistence, redirects, and SSL. Be aware of version compatibility and OS differences.

## See Also (Дивіться також)

- Libcurl Documentation: https://curl.se/libcurl/c/
- HTTP Protocol Overview: https://developer.mozilla.org/en-US/docs/Web/HTTP
- C Network Programming with Sockets: https://beej.us/guide/bgnet/

Check these out for a broader understanding or different perspectives on HTTP requests in C.
