---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

HTTP请求是让一个程序与其它服务器进行交互的一种方式。程序员常用它来向服务器获取信息或者发送数据。

## 怎么做：

在C中，你可以使用 libcurl 库来发送HTTP请求。以下是一个简单的例子，向 `http://example.com` 发送GET请求：

```C
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

这会返回来自 `http://example.com` 的HTML内容。

## 深入探讨

HTTP请求的历史始于1991年，那时候只包括GET和POST两种方法。如今，已经演变出多种方法，比如PUT、DELETE等用来满足更复杂的需求。

libcurl是一种著名的选择，但还有其他库，比如wininet（在windows平台中），或者你也可以直接使用socket。

libcurl可以处理各种类型的HTTP请求，不仅仅是GET请求。你可以通过设置 `CURLOPT_CUSTOMREQUEST` 选项来发送任何类型的HTTP请求。

## 另请参阅

`libcurl` 文档: https://curl.haxx.se/libcurl/c/

HTTP方法指南: https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods