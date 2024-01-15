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

## 为什么

当我们在浏览互联网上的网页时，有时会想保存或下载网页，以便将来参考或离线浏览。使用C++编程可以帮助我们实现这个目的，使我们能够轻松地将网页下载到本地存储。

## 如何进行

首先，我们需要了解如何使用C++中的库来进行网络请求。以下是一个基本的代码示例，显示如何使用C++中的`http`和`fstream`库来发送GET请求并将响应保存到本地文件中。

```C++
#include <iostream>
#include <fstream>
#include <curl/curl.h>

using namespace std;

//回调函数，用于将响应保存到本地文件中
size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
   size_t written = fwrite(ptr, size, nmemb, stream);
   return written;
}

//主函数
int main() {
   //声明一个CURL句柄
   CURL *curl;
   //声明一个文件指针，用于保存响应
   FILE *fp;
   //设置要下载的网页URL
   string url = "https://example.com";
   
   //初始化CURL句柄
   curl = curl_easy_init();
   if(curl) {
      //设置要请求的网页URL
      curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
      //设置回调函数
      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
      //设置响应保存的文件
      fp = fopen("example.html","wb");
      if(fp) {
         //设置响应保存的文件流
         curl_easy_setopt(curl,   CURLOPT_WRITEDATA, fp);
         //执行请求
         curl_easy_perform(curl);
         //关闭文件指针
         fclose(fp);
      }
      //清除CURL句柄
      curl_easy_cleanup(curl);
   }
   
   return 0;
}
```

执行以上代码后，将会在同级目录下生成`example.html`文件，它就是我们下载的网页内容。

## 深入了解

上面的代码示例只是实现了最基本的网页下载功能。如果我们想要实现更多的功能，比如下载特定网页的特定部分，或者下载多个网页并生成一个HTML文档等，就需要更深入地了解网络请求和HTML解析的相关知识。

可以使用第三方库，比如`libcurl`和`libxml2`来帮助我们实现更复杂的功能。同时，也可以学习网络协议和HTML标记语言的相关知识，以便更好地理解网页下载的原理。

## 查看更多资源

- [C++官方网站](https://isocpp.org/)
- [libcurl官方文档](https://curl.se/libcurl/)
- [libxml2官方文档](http://www.xmlsoft.org/)
- [W3School-HTML教程](https://www.w3school.com.cn/html/)
- [菜鸟教程-C++教程](https://www.runoob.com/cplusplus/cpp-tutorial.html)