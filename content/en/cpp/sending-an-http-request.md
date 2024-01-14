---
title:                "C++ recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is an essential task in building modern web applications. It allows our application to communicate with servers and retrieve data, making the user experience more dynamic and interactive. In this blog post, we will explore how to send HTTP requests using C++.

## How To

To send an HTTP request using C++, we will use the popular open-source library called cURL. It provides a simple interface to make HTTP requests and supports various protocols, including HTTP, HTTPS, FTP, and more.

First, we need to include the cURL library in our C++ code using the `#include <curl/curl.h>` directive. Next, we will initialize the cURL session using `curl_easy_init()` function and assign it to a `CURL` variable. Then we will set the URL we want to make a request to using the `curl_easy_setopt()` function.

```C++
// initialize cURL session
CURL *curl = curl_easy_init();
// set URL for HTTP request
curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
```

Now, we can add any additional options to our request, such as setting headers or request parameters, using the same `curl_easy_setopt()` function. After that, we can perform the request using the `curl_easy_perform()` function. And finally, we can retrieve the response using the `CURLOPT_WRITEFUNCTION` option and writing the response data to a file using the `FILE` type.

```C++
// set headers for request
struct curl_slist *headers = NULL;
headers = curl_slist_append(headers, "Content-Type: application/json");
curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

// perform request
CURLcode res = curl_easy_perform(curl);

// retrieve response
FILE *fp = fopen("response.txt", "wb");
curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
curl_easy_perform(curl);
fclose(fp);

// cleanup cURL session
curl_easy_cleanup(curl);
```

After executing the code, we will get a `response.txt` file with the response data from the server.

## Deep Dive

In the above example, we used the `cURL` library's easy interface, which is a high-level API. We can also use a lower-level API provided by the cURL library, which allows us to have more control over the HTTP request and response. The lower-level API is called the `multi` interface, and it allows us to perform multiple requests simultaneously.

Another important aspect of sending HTTP requests is handling errors and retries. The cURL library provides built-in functionality to handle errors and retries using the `CURLOPT_ERRORBUFFER`, `CURLOPT_FAILONERROR`, and `CURLOPT_REDIR_PROTOCOLS` options.

Moreover, cURL also supports asynchronous requests using the `curl_multi` interface, allowing us to make multiple requests in parallel and handle them asynchronously.

## See Also

- [Official cURL website](https://curl.se)
- [cURL documentation](https://curl.se/libcurl/)
- [cURL GitHub repository](https://github.com/curl/curl)