---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:43:26.004664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Downloading a web page means fetching its HTML content via the internet. Programmers do this to interact with the web programmatically, either to scrape data, test APIs, or serve as part of a backend process.

## How to: (Як це зробити:)
```C
#include <stdio.h>
#include <curl/curl.h>

static size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main(void) {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            printf("%s\n", readBuffer.c_str());
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Make sure to include `libcurl` in your project. The code performs a simple HTTP GET request to download a web page and prints it out.

Sample output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (Поглиблений Аналіз):
Historically, downloading web content in C wasn't as straightforward as using higher-level languages. The libcurl library made it much easier, providing functions for various protocols like HTTP, HTTPS, FTP, etc. Alternatives to libcurl are socket programming (more complex, low-level control) and using other libraries like POCO or Boost.Asio (not as commonly used as libcurl). When implementing a web page downloader, consider error handling, redirects, timeouts, and proper memory management to avoid leaks.

## See Also (Дивіться також):
- [libcurl](https://curl.se/libcurl/)
- [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/)
- [Curl tutorial for C/C++ programmers](http://www.cplusplus.com/reference/string/string/c_str/)
- [Socket Programming in C/C++](https://www.geeksforgeeks.org/socket-programming-cc/)
