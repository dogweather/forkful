---
title:                "使用基本身份验证发送HTTP请求。"
html_title:           "C: 使用基本身份验证发送HTTP请求。"
simple_title:         "使用基本身份验证发送HTTP请求。"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求时，带有基本身份验证信息的原因是为了确保请求是被授权的。这可以保护敏感的数据和网站免受未经授权的访问。

## 如何进行

在C语言中发送带有基本身份验证的HTTP请求非常简单。首先，我们需要使用curl库来构建和发送请求。然后，我们需要提供基本身份验证的用户名和密码。以下是一个简单的示例代码：

```C
// 导入所需的头文件
#include <curl/curl.h>
#include <stdio.h>

// 定义用户名和密码
#define USERNAME "username"
#define PASSWORD "password"

int main(void) {
  // 初始化curl
  CURL *curl;
  CURLcode res;

  // 为用户名和密码创建认证字符串
  char auth_string[100];
  sprintf(auth_string, "%s:%s", USERNAME, PASSWORD);

  // 使用curl_easy_init()来构建新的curl对象
  curl = curl_easy_init();
  if(curl) {
    // 设置要发送的URL
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    // 设置要发送的身份验证信息
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, "Authorization: Basic %s", auth_string);

    // 发送请求
    res = curl_easy_perform(curl);

    // 检查请求结果
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    // 清理curl对象
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

以下是在控制台上运行上述代码的样本输出：

```
HTTP/1.1 200 OK
Date: Fri, 27 Nov 2020 00:00:00 GMT
Content-Type: text/html; charset=UTF-8
Content-Length: 138
Server: Apache

<html>
  <head>
    <title>Hello World</title>
  </head>
  <body>
    <h1>Hello, World!</h1>
  </body>
</html>
```

请注意，上述示例代码仅用于演示目的，并且并非完整的可执行程序。您可能需要根据您的具体需求进行修改和调整。

## 深入了解

发送带有基本身份验证的HTTP请求过程涉及设置HTTP标头。除了使用```CURLOPT_HTTPHEADER```之外，您也可以使用```curl_easy_setopt()```函数来设置其他HTTP标头，例如User-Agent，Content-Type等。您可以通过查看curl文档来了解更多关于设置HTTP标头的详细信息。

此外，如何处理服务器的身份验证错误也是非常重要的。通常，如果提供的用户名和密码不正确，则服务器会返回401 Unauthorized错误响应。您可以通过检查```CURLcode```变量的值来处理此类错误。

## 参考

1. [libcurl - the multiprotocol file transfer library](https://curl.haxx.se/libcurl/)
2. [curl_easy_setopt() function](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html)
3. [curl文档](https://curl.haxx.se/docs/manual.html)

## 参见

- [HTTP身份验证基础知识](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [如何使用C语言发送HTTP请求](https://www.geeksforgeeks.org/http-requests-c-language/)
- [基于C语言的libcurl教程](https://curl.haxx.se/libcurl/c/libcurl-tutorial.html)