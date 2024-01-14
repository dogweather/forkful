---
title:                "C++: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么要发送HTTP请求
在现今的数字时代，我们经常需要从不同的网站和服务器获取数据。而发送HTTP请求可以帮助我们与这些网站和服务器进行通信，从而获得我们想要的数据。无论是开发网络应用程序，还是做数据分析，发送HTTP请求都是必不可少的。

## 如何发送HTTP请求

```C++
#include <iostream>
#include <curl/curl.h> //需要安装libcurl库

int main() {
  CURL *curl;
  CURLcode res;
  std::string url = "https://www.example.com"; //设置请求的网址
  curl = curl_easy_init(); //初始化

  if (curl) {
    //设置请求的选项
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str()); //设置请求的网址
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L); //设置是否跟随重定向
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); //设置接收响应的回调函数（这里为空）
    
    //发送请求并获取响应
    res = curl_easy_perform(curl); //执行请求
    if (res != CURLE_OK) {
      std::cout << "Error: " << curl_easy_strerror(res) << std::endl; //打印错误信息
    }
 
    //获取响应的状态码
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code); //获取响应的信息
    std::cout << "HTTP Response Code: " << http_code << std::endl;
    
    //关闭会话
    curl_easy_cleanup(curl);

    return 0;
  }
}
```

输出：
```
HTTP Response Code: 200
```

## 深入了解发送HTTP请求

发送HTTP请求需要使用第三方库，比如上面示例中用到的libcurl。libcurl是一个强大的网络传输库，可以帮助我们方便地发送各种类型的HTTP请求，并处理返回的响应。它支持多种协议，包括HTTP、HTTPS、FTP等。

在发送HTTP请求时，我们可以通过设置不同的选项来定制我们的请求，比如设置请求方法、请求头、请求体等。同时，libcurl也提供了丰富的回调函数来处理响应，比如保存响应为文件、输出到终端等。

## 尝试一下吧

希望这篇文章能够帮助你了解如何通过C++发送HTTP请求。如果想要深入了解libcurl的更多特性，可以参考官方文档和示例代码。

## 查看更多

- [libcurl官方文档](https://curl.haxx.se/libcurl/c/)
- [libcurl GitHub仓库](https://github.com/curl/curl)
- [使用libcurl发送GET请求的示例代码](https://curl.haxx.se/libcurl/c/simple.html)