---
title:                "Downloading a web page"
html_title:           "C recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why: 
Downloading a webpage is a common task in web development, as it allows you to gather and manipulate data from the web for various purposes. Whether you are creating a web scraper or building an application that needs to access web content, knowing how to download a webpage in C can come in handy.

## How To:
To download a webpage in C, we will be using the libcurl library, a popular and powerful open-source library for transferring data over various network protocols. First, we need to include the header file for libcurl in our program:
```C
#include <curl/curl.h>
```
Next, we will create a CURL object and set the URL of the webpage we want to download:
```C
CURL *curl;
curl = curl_easy_init();
curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
```
Then, we can use the CURLOPT_WRITEFUNCTION option to set a callback function that will be called as soon as the webpage data is received. This function will receive the data in chunks, which we can then manipulate or store in a variable. For example, we can simply print out the received data to the console:
```C
static size_t print_data(void *ptr, size_t size, size_t nmemb, void *userp) {
    printf("%s", (char*)ptr);
    return size * nmemb;
}
// set the callback function
curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, print_data);
```
Finally, we can perform the download using the curl_easy_perform function and then clean up our CURL object:
```C
curl_easy_perform(curl);
curl_easy_cleanup(curl);
```
If we run our program, we should see the webpage data printed to the console in chunks as it is received.

## Deep Dive:
The libcurl library provides many options and functions for customizing and managing downloads, such as setting headers, handling errors, and following redirects. It also supports synchronous and asynchronous downloads. For a more in-depth look at all the available features, check out the official documentation for libcurl.

## See Also:
- [libcurl documentation](https://curl.haxx.se/libcurl/)
- [libcurl GitHub repository](https://github.com/curl/curl)
- [How to use libcurl](https://daniel.haxx.se/libcurl/)