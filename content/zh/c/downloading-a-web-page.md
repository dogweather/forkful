---
title:                "下载网页"
aliases:
- zh/c/downloading-a-web-page.md
date:                  2024-02-03T17:55:58.882371-07:00
model:                 gpt-4-0125-preview
simple_title:         "下载网页"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

在C语言中下载网页涉及到程序性地访问互联网上的网页内容，并将其保存在本地以供处理或离线使用。程序员常常进行这样的操作，以消费网络服务、爬取网页内容，或直接从他们的应用程序与在线资源进行交云。

## 如何操作：

在C语言中下载一个网页的一种流行方法是使用libcurl库，它是一个高效且可移植的客户端URL传送库。确保你的项目中安装并链接了libcurl。这里有一个示例，演示了如何使用libcurl来下载网页的内容：

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // 初始化一个libcurl简单会话
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // 用于写入接收数据的回调
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // 设置文件指针以写入数据

        res = curl_easy_perform(curl); // 执行文件下载
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* 总是需要清理 */
        curl_easy_cleanup(curl); // 清理简单会话
        fclose(fp); // 关闭文件流
    }
    return 0;
}
```
示例输出（控制台无可见输出）：此代码下载指定URL的内容，并将其保存为一个名为`downloaded_page.html`的文件。检查你的程序目录下的这个文件，以查看下载的内容。

## 深入探讨：

从历史上看，在C语言中下载网页内容更加繁琐，需要手动的套接字编程和HTTP协议处理。Libcurl抽象了这些复杂性，提供了一个健壮且高级的API进行网络上的数据传输。

虽然libcurl在C中简化了HTTP请求，但像Python与其`requests`库或JavaScript (Node.js)与各种HTTP客户端库这样的现代编程语言可能提供更直观的语法和对JSON及其他常用于网络通信的数据格式的内置支持。然而，C与libcurl提供了一个高性能且稳定的解决方案，对于那些效率、细粒度控制或集成到现有C代码库至关重要的系统来说。同时值得注意的是，C结合libcurl不仅仅可以用来下载网页——它还能处理FTP、SMTP等更多任务，使其成为程序员工具箱中的多才多艺工具。
