---
title:                "发送HTTP请求"
html_title:           "C++: 发送HTTP请求"
simple_title:         "发送HTTP请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

这篇文章将带你深入了解如何使用 C++ 发送 HTTP 请求。通过发送 HTTP 请求，我们可以与网站、服务器或其他计算机进行通信，从而以交互式的方式获取数据和信息。这是构建现代应用程序所必需的基础知识。

## 如何进行

首先，我们需要引入一个 C++ 的库来处理网络请求，如 [libcurl](https://curl.se/libcurl/)。接下来，我们需要设置一个 [HTTP 请求](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods) ，例如 GET、POST 或 PUT。然后，我们需要指定目标 URL，并添加任何必要的请求头和参数。最后，我们使用 [libcurl](https://curl.se/libcurl/) 的函数来发送请求并获取响应状态码和数据。

```C++
// 引入 libcurl 头文件
#include <curl/curl.h>

// 创建 HTTP 请求，此处以 GET 请求为例
CURL* curl = curl_easy_init();
// 设置请求 URL
curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api");
// 添加必要的请求头和参数
struct curl_slist *headers = NULL;
headers = curl_slist_append(headers, "Content-Type: application/json");
curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
// 发送请求并获取响应
CURLcode res = curl_easy_perform(curl);
long http_code;
curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
if (http_code == 200) {
    // 请求成功，处理响应数据
    ...
} else {
    // 请求失败，处理错误信息
    ...
}
```

## 深入了解

发送 HTTP 请求本质上是通过 TCP 协议与目标主机的端口建立连接，并按照 HTTP 协议规定的格式发送请求信息。随后目标主机会返回 HTTP 响应，包含响应状态码、响应头和响应数据。使用 C++ 可以轻松地处理这些信息，并基于网络协议构建强大的应用程序。

### 维持连接

在发送多个 HTTP 请求时，可以复用同一个连接（使用 [libcurl](https://curl.se/libcurl/) 的 `CURL*`）来提高效率。只需要在每次发送请求前，重置请求头和标识符，即可保持同一个连接。但是需要注意的是，如果请求的目标主机不同，还是需要重新建立连接。

### 异步请求

使用 [libcurl](https://curl.se/libcurl/) 的 `CURLM` API，可以实现发送 HTTP 请求的异步处理，从而提高应用程序的并发性能。这种方式适用于发送大量请求，且每个请求可以独立处理响应的情况。

## 参考链接

- [libcurl 官方文档](https://curl.se/libcurl/)
- [HTTP 请求方法](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods)
- [C++ 中的网络编程](https://www.geeksforgeeks.org/network-programming-in-c-c/)