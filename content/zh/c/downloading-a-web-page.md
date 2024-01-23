---
title:                "下载网页"
date:                  2024-01-20T17:43:20.995918-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? / 什么和为什么？

下载网页就是从互联网上获取网页的内容。程序员这么做是为了处理或分析网页数据。

## How to: / 怎么做：

使用C库比如libcurl进行网页下载：

```c
#include <stdio.h>
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "downloaded_page.html";

    curl = curl_easy_init();
    if(curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        /* Always cleanup */
        curl_easy_cleanup(curl);
        fclose(fp);

        if (res == CURLE_OK)
            printf("Download successful.\n");
        else
            printf("Download failed: %s\n", curl_easy_strerror(res));
    }
    return 0;
}
```
编译时记得链接libcurl：

```bash
gcc -o download download.c -lcurl
```

运行后会看到 `Download successful.` 并在当前目录找到下载的HTML文件。

## Deep Dive / 深入研究：

早年，下载网页通常采用HTTP协议，手写socket编程。现今，libcurl库是一种流行的方法，支持HTTPS，更安全。但libcurl不是C语言标准库的一部分，需要先安装。可以直接读取内存数据，或者保存到文件。其他方法如WinINet针对Windows，但libcurl跨平台。

## See Also / 另请参阅：

- libcurl官网: https://curl.se/libcurl/
- cURL命令行工具教程: https://curl.se/docs/manual.html
- HTTP协议介绍: https://www.w3.org/Protocols/
