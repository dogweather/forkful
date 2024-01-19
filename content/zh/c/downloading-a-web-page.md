---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么与为何？

下载网页就是将互联网上的一整个网页保存到本地设备上。程序员下载网页以备离线浏览，进行数据抓取，或对网页内容加以分析和使用。

## 怎么做：

以下是一个C语言实现下载网页的代码示例。在此示例中，我们使用libcurl库来下载网页。

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

#ifndef NDEBUG
    curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);
#endif

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
执行此代码，您将在终端看到对应的HTML输出。这就是网页内容。

## 深入探究

1. 历史背景：早期的网页获取方法主要依赖于TCP/IP socket编程，然后发送HTTP请求并接收响应。然后libcurl库的出现，让这个过程变得更加简单。
2. 替代方案：除了libcurl，还有其他库，如wget，也可以用来轻松下载网页。
3. 实现细节：下载网页的主要步骤包括创建一个HTTP请求，发送请求，然后接收并解析响应。libcurl方法包括了这些步骤的所有细节。

## 还可参阅

+ libcurl官方文档 [https://curl.haxx.se/libcurl/](https://curl.haxx.se/libcurl/)
+ C语言socket编程 [http://beej.us/guide/bgnet/](http://beej.us/guide/bgnet/)
+ wget官方文档 [https://www.gnu.org/software/wget/manual/](https://www.gnu.org/software/wget/manual/)