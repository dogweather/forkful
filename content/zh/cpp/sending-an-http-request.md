---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么是 HTTP 请求 & 为什么使用？

在计算机编程里，发起 HTTP 请求表示您的程序正在尝试从服务器获取某些信息或进行某些操作。程序员之所以这样做，是因为许多应用程序的功能依赖于从网络获取数据，如天气应用，社交媒体应用等。

## 怎么操作：

以下是使用 C++ 发送 HTTP GET 请求的代码示例和输出结果。

```C++
// 编译需要包含这些库
#include <curl/curl.h>
#include <string>

// CURL 的写回调函数
size_t curl_write(void* buffer, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)buffer, size * nmemb);
    return size * nmemb;
}

// 使用 CURL 发送 HTTP GET 请求
void get_http(std::string url, std::string &response) {
    CURL* curl = curl_easy_init();

    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_write);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
 
    curl_easy_perform(curl);
    curl_easy_cleanup(curl);
}
```

## 深度理解

1. **历史背景**：HTTP 请求最初在 1990 年代随着互联网的诞生而产生，为了在网页浏览器和服务器之间传输信息。
2. **可选方法**：除了使用 libcurl 库，还可以使用 POCO, Boost, C++ Request等其他 C++ 库来发送 HTTP 请求。
3. **运行原理**：发送 HTTP 请求时，您的程序会与服务器建立一个网络连接，然后以特定格式发送信息，等待服务器的响应。

## 参考资料

- [libcurl C++ 文档](https://curl.se/libcurl/c/)
- [POCO Network 章节](https://pocoproject.org/docs/)
- [Boost 库](https://www.boost.org/)
- [C++ Requests: Curl for People](https://github.com/whoshuu/cpr)