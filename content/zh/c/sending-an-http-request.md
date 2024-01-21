---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:10.187235-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)

发送HTTP请求就是让你的程序能在网上"说话"，向服务器请求信息。程序员这样做是为了从网页获取数据，或者是和网络服务交互。

## How to: (如何操作：)

下面是一个用C发送HTTP GET请求的简短例子，使用了`libcurl`库：

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
当你运行这段代码，如果成功，你会看到`example.com`的HTML内容输出到屏幕上。

## Deep Dive (深入探索)

### 历史背景
在C语言中发送HTTP请求并没有内建的支持，因为C是在网络普及前就开发的。所以，需要使用像`libcurl`这样的库，这是一个支持多协议的轻量级传输库。

### 替代方法
除了`libcurl`，你也可以使用其他库比如`libhttp`或者完全自己手动使用sockets进行HTTP协议的通信。

### 实现细节
`libcurl`封装了底层的网络操作，让发送HTTP请求变得简单。通过设置不同的选项，可以执行GET, POST请求等操作，并处理响应数据。

## See Also (另见)

- libcurl 官网: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- HTTP 协议详解: [https://www.rfc-editor.org/rfc/rfc2616](https://www.rfc-editor.org/rfc/rfc2616)
- C语言网络编程入门: [https://beej.us/guide/bgnet/](https://beej.us/guide/bgnet/)