---
title:                "使用基本身份验证发送http请求。"
html_title:           "C: 使用基本身份验证发送http请求。"
simple_title:         "使用基本身份验证发送http请求。"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么是基本身份验证的HTTP请求？为什么程序员要这样做？

基本身份验证的HTTP请求是一种通过发送认证信息来访问网络资源的方法。程序员经常使用它来保护网络应用程序免受未经授权的访问。这样做可以确保只有经过身份验证的用户才能访问受保护的资源，从而提高安全性。

## 如何实现基本身份验证的HTTP请求？ 

下面是一个简单的代码示例，演示如何使用libcurl库发送带有基本身份验证的HTTP请求：

```C
#include <stdio.h>  
#include <curl/curl.h>  
  
int main(void)  
{  
    CURL *curl;  
    CURLcode res;  
  
    curl = curl_easy_init();  
    if(curl) {  
        // 设置基本认证的用户名和密码  
        curl_easy_setopt(curl, CURLOPT_USERPWD, "[username]:[password]");  
        // 设置基本认证的认证方式  
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);  
        // 设置URL  
        curl_easy_setopt(curl, CURLOPT_URL, "[URL]");  
        // 执行HTTP请求  
        res = curl_easy_perform(curl);  
  
        // 检查请求是否成功  
        if(res == CURLE_OK)  
            printf("HTTP 请求执行成功！\n");  
        else  
            fprintf(stderr, "HTTP 请求执行失败！\n");  
  
        // 清除curl句柄  
        curl_easy_cleanup(curl);  
    }  
    return 0;  
}  
```

运行以上代码，你将得到类似以下的输出：

```
HTTP 请求执行成功！
```

## 深入探讨

基本身份验证的HTTP请求是在1999年被RFC 2617标准化的。除了基本身份验证外，还有其他几种HTTP身份验证方式，例如摘要身份验证和OAuth。程序员可以根据需要选择使用哪种身份验证方式，基本身份验证在一些场景中可能存在安全性问题，因为它只是简单地将用户名和密码以明文形式发送给服务器。

当使用基本身份验证时，程序员还需要考虑如何处理服务器返回的认证错误，例如用户名或密码错误的情况。一般来说，服务器会返回一个401错误码，并提供一个认证验证(Challenge)。程序员需要解析并处理这个验证信息，然后重新发送带有正确身份信息的HTTP请求。

## 阅读更多

想要了解更多关于基本身份验证的HTTP请求的信息，可以阅读这些资源：

- [RFC 2617: HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [libcurl library](https://curl.haxx.se/libcurl/)
- [C语言从入门到精通](https://book.douban.com/subject/10538373/)