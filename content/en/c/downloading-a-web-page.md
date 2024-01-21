---
title:                "Downloading a web page"
date:                  2024-01-20T17:43:42.248084-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page means fetching its HTML content from the web server it resides on. Programmers do this to process, analyze, or interact with the web page's data offline.

## How to:
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "downloaded_page.html";

    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        /* Check for errors */
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
        
        /* Clean up */
        curl_easy_cleanup(curl);
        fclose(fp);
    }

    return 0;
}
```
Sample output:
```
(No output, but check the current directory for a 'downloaded_page.html' file)
```

## Deep Dive
Back in the early days of the internet, grabbing a web page involved raw HTTP requests through TCP sockets - cumbersome, to say the least. These days, we've got libraries like libcurl, which takes the grunt work out of the process. It handles all the nitty-gritty of HTTP requests, SSL connections, and more.

There are a few alternatives to libcurl like wget and http-client in C, but libcurl is widely used for its robustness and features. When using libcurl, keep this in mind:

- Initialization with `curl_easy_init()` is a must.
- Set options suitable for your needs; for downloading, we need to specify the URL and the write function.
- `CURLOPT_WRITEFUNCTION` allows us to pass a pointer to our callback function to write the data to a file.
- Always check the result of `curl_easy_perform()` for errors.
- Don't forget to clean up with `curl_easy_cleanup()` to prevent leaks.

For production code, you'd want error handling, check HTTP status codes, and manage security considerations (like SSL certificate verification).

## See Also
- [libcurl](https://curl.se/libcurl/)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)