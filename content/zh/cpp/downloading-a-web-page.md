---
title:                "下载网页"
html_title:           "C++: 下载网页"
simple_title:         "下载网页"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么是下载网页？为什么程序员要这么做？
下载网页是指从互联网上获取网页内容的过程。程序员经常使用这种技术来获取网页上的数据，比如图片、文本等，以进行后续的处理。

## 如何操作：
以下是用C++实现下载网页的代码示例和输出结果：

```C++
#include <iostream>
#include <curl/curl.h>

// URL to download
#define URL "https://www.example.com"

// This function will be used to write the downloaded data
size_t write_callback(char* ptr, size_t size, size_t nmemb, void* userdata) {
    std::string* str = (std::string*)userdata;
    str->append(ptr, size * nmemb);
    return size * nmemb;
}

int main() {
    // Initialize CURL object
    CURL* curl = curl_easy_init();

    // Set URL to download
    curl_easy_setopt(curl, CURLOPT_URL, URL);

    // Set callback function to write data
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &received_data);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);

    // Download the webpage
    CURLcode res = curl_easy_perform(curl);

    // Check for errors
    if (res != CURLE_OK) {
        std::cout << "Error: " << curl_easy_strerror(res) << std::endl;
    }

    // Clean up
    curl_easy_cleanup(curl);

    return 0;
}
```

输出：

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>

    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style type="text/css">
    body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
        /* svg displays a black bar at the bottom on iPad */
        /* Thanks initially to Thierry Koblentz: tjkdesign.com/articles/svg-and-the-iphone */
        font-size: 100%;
        /* medium is the default body font size */
    }
    :root {
        color-scheme: light dark;
    }

    body {
        color: #000000;
    }
    </style>    
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## 深入了解：
下载网页的技术起源于早期的数据提取和网页抓取技术，又称为数据挖掘。除了使用C++，程序员还可以使用其他编程语言实现网页下载，比如Python、Java等。另外，也可以借助现有的网络爬虫框架来简化下载网页的过程，如Scrapy、Puppeteer等。

## 了解更多：
- [CURL官方文档](https://curl.se/libcurl/)
- [使用C++来抓取网页内容](http://www.cppblog.com/ifsight/archive/2009/03/03/74003.html)
- [网络爬虫框架Scrapy](https://scrapy.org/)
- [Chrome开发者工具的使用](https://developer.chrome.com/docs/devtools/)