---
title:                "-使用基本认证发送HTTP请求"
html_title:           "C++: -使用基本认证发送HTTP请求"
simple_title:         "-使用基本认证发送HTTP请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

在建立网络连接时，服务器通常会要求用户提供身份验证信息，以确保安全性。HTTP请求中的基本身份验证允许用户通过提供用户名和密码来验证其身份，从而向服务器发送请求。

## 如何发送带基本身份验证的HTTP请求

```C++
//导入必要的头文件
#include <iostream>
#include <curl/curl.h>

//main函数
int main ()
{
  //url包含基本身份验证信息
  char* url = "https://example.com/";

  CURL *curl;
  CURLcode res;

  //初始化curl
  curl = curl_easy_init();
  if(curl) {
    //设置用于验证的用户名和密码
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
    //设置url
    curl_easy_setopt(curl, CURLOPT_URL, url);
    //发送请求
    res = curl_easy_perform(curl);
    //检查请求是否成功
    if(res != CURLE_OK)
        fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    //清除curl相关信息
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

输出结果应为：

```C++
<html>
<head>
    <title>Example Domain</title>
</head>
<body>
    <h1>HTTP Basic Authentication Successful</h1>
</body>
</html>
```

## 深入了解

在HTTP请求中，基本身份验证是最简单和最常用的身份验证方法。它是一种无状态的认证机制，即每次请求都需要提供用户名和密码，服务器不会保存这些信息。用户名和密码在所有请求中都以明文的方式传输，因此建议在使用HTTPS协议时使用基本身份验证。

## 看看此外

- [CURL官方文档](https://curl.haxx.se/libcurl/c/CURLOPT_USERPWD.html)
- [HTTP基本身份验证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [C++编程指南](https://www.cplusplus.com/doc/)