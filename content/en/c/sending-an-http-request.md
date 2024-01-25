---
title:                "Sending an HTTP request"
date:                  2024-01-20T17:59:09.673014-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is how your program asks for data or sends data to a web server. Programmers use it to interact with APIs, grab web content, or communicate with other services.

## How to:

In C, we'll leverage `libcurl` for this task. It's a powerful library for transferring data with URLs. First, get `libcurl` installed. On Debian-based systems, you could use `sudo apt-get install libcurl4-openssl-dev`.

Here's a snippet for making a simple GET request:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        /* Perform the request, res will get the return code */ 
        res = curl_easy_perform(curl);
        
        /* Check for errors */ 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        
        /* always cleanup */ 
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Running this, you won't get a visible output as we haven't handled the response, but it'll fetch the content from `http://example.com`.

## Deep Dive

`libcurl` started in 1997 and has become a go-to library for C programmers. It supports a vast number of protocols, not just HTTP. Alternatives for HTTP in C might include writing your own implementation, but that’s a bumpy ride through socket programming and complex RFCs.

`libcurl` is convenient because it handles all the nitty-gritty details for you, like protocol negotiation, error handling, and data transfer. Plus, it's cross-platform—use the same code on Linux, Windows, Mac, you name it.

Remember, `libcurl` uses a synchronous API by default, which might block your main thread. If you're building something where that matters, you might have to dive into multi-threading or the `curl_multi_*` set of asynchronous functions.

## See Also

- Official libcurl website for documentation and examples: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- HTTP protocol details for background knowledge: [https://www.ietf.org/rfc/rfc2616.txt](https://www.ietf.org/rfc/rfc2616.txt)
- For broader perspectives on C network programming: [Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)