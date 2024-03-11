---
date: 2024-01-20 18:01:31.202502-07:00
description: "\u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u8BA9\
  \u4F60\u7684\u5E94\u7528\u53EF\u4EE5\u5B89\u5168\u8BBF\u95EE\u9700\u8981\u7528\u6237\
  \u540D\u548C\u5BC6\u7801\u7684\u8D44\u6E90\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u4E3A\u4E86\u4FDD\u62A4\u6570\u636E\uFF0C\u907F\u514D\u672A\u6388\u6743\u8BBF\u95EE\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.910924-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u8BA9\u4F60\
  \u7684\u5E94\u7528\u53EF\u4EE5\u5B89\u5168\u8BBF\u95EE\u9700\u8981\u7528\u6237\u540D\
  \u548C\u5BC6\u7801\u7684\u8D44\u6E90\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\
  \u4E86\u4FDD\u62A4\u6570\u636E\uFF0C\u907F\u514D\u672A\u6388\u6743\u8BBF\u95EE\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)

发送带基本认证的HTTP请求让你的应用可以安全访问需要用户名和密码的资源。程序员这么做为了保护数据，避免未授权访问。

## How to: (如何实现：)

```C++
#include <iostream>
#include <curl/curl.h>
#include <string>

// 初始化CURL，设置基本认证信息，发送GET请求，并清理
void sendGetRequestWithBasicAuth(const std::string& url, 
                                 const std::string& username, 
                                 const std::string& password) {
    CURL *curl = curl_easy_init();
    if(curl) {
        CURLcode res;
        std::string userPass = username + ":" + password;
        // 设置目标URL和认证信息
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPass.c_str());
        
        // 发送请求
        res = curl_easy_perform(curl);
        
        if(res != CURLE_OK) {
            std::cerr << "CURL error: " << curl_easy_strerror(res) << '\n';
        }
        
        // 清理
        curl_easy_cleanup(curl);
    }
}

int main() {
    std::string url = "http://yourapi.com/data";
    std::string username = "user";
    std::string password = "pass";
    
    sendGetRequestWithBasicAuth(url, username, password);
    
    return 0;
}
```
这段代码没有输出。成功时，它默默工作。失败时，会在控制台打印错误信息。

## Deep Dive (深入探究):

发送带基本认证的HTTP请求是互联网早期常用的一种认证方式，它简单但不是最安全。基本认证通过编码'用户名:密码'（并不是加密！）并附加在请求头上。现代web更推荐使用基于token的方法，比如OAuth。

C++中发送HTTP请求可以用不同库，比如CURL或Boost.Beast。CURL是C语言库，但可以很好地和C++集成。不同于CURL，Boost.Beast是纯C++，利用了现代C++特性。选择合适的库取决于项目需求和个人偏好。

确保你了解了HTTPS和加密的重要性。基本认证如果不通过HTTPS发送，用户名和密码可能被拦截。

## See Also (另请参阅):

- CURL官方文档: https://curl.se/libcurl/c/libcurl.html
- Boost.Beast库: https://www.boost.org/doc/libs/release/libs/beast/
- RFC 7617 The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- OAuth 2.0: https://oauth.net/2/
