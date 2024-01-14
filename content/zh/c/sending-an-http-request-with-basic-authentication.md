---
title:                "C: 使用基本身份验证发送一个HTTP请求"
simple_title:         "使用基本身份验证发送一个HTTP请求"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么要使用基本认证发送HTTP请求

基本认证是一种简单的安全机制，可以保护HTTP请求的隐私和安全性。通过使用基本认证，用户可以在发送HTTP请求时提供用户名和密码，从而验证其身份，并且只有经过身份验证的用户才能访问受保护的资源。

## 如何发送带有基本认证的HTTP请求

如果您想在C语言中发送带有基本认证的HTTP请求，您可以遵循以下步骤：

1. 创建一个包含基本认证头部的HTTP请求对象
2. 设置请求方法和URL
3. 将用户名和密码编码为Base64字符串，并将其添加到HTTP头部中
4. 发送HTTP请求并接收响应
5. 检查响应状态码和内容

下面是一个示例代码，展示如何使用libcurl库在C语言中发送带有基本认证的HTTP请求，并打印出响应：

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    /* 初始化libcurl */
    curl = curl_easy_init();
    if(curl) {
        /* 设置URL和要发送的用户名密码 */
        char *url = "https://example.com/api";
        char *username = "user123";
        char *password = "password456";
        char auth[100];
        sprintf(auth, "%s:%s", username, password);

        /* 创建HTTP请求对象 */
        curl_easy_setopt(curl, CURLOPT_URL, url);

        /* 添加基本认证头部 */
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, auth);

        /* 发送HTTP请求 */
        res = curl_easy_perform(curl);

        /* 检查响应状态码 */
        long status;
        curl_easy_getinfo(curl, CURLINFO_RESPONSECODE, &status);
        if (status == 200) {
            /* 如果响应成功，则读取响应内容并打印出来 */
            char *response;
            curl_easy_getinfo(curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD,
                              &size);
            response = (char*) malloc((int)size * sizeof(char));
            curl_easy_getinfo(curl, CURLINFO_CONTENT_TYPE, &type);
            curl_easy_getinfo(curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD,
                              &size);
            response = realloc(response, size + 1);
            curl_easy_getinfo(curl, CURLINFO_CONTENT_TYPE, &type);
            printf("响应内容类型:%s\n", type);
            printf("响应内容长度:%d\n", size);
            printf("响应内容:\n%s", response);
        } else {
            /* 如果响应失败，则打印出错误信息 */
            fprintf(stderr, "HTTP请求失败，错误码: %ld\n", (long)status);
        }

        /* 释放资源 */
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

上面的代码中，我们使用libcurl库创建了一个带有基本认证头部的HTTP请求对象，并发送了一个HTTP请求到https://example.com/api。我们还对响应状态码进行了检查，如果响应成功则读取响应内容并打印出来，如果响应失败则打印出错误信息。

## 深入了解发送带有基本认证的HTTP请求

当我们使用基本认证发送HTTP请求时，服务器会返回一个401 Unauthorized的响应（如果请求没有提供用户名和密码）。客户端收到此响应后，会重新发送带有认证信息的HTTP请求。基本认证的用户名和密码信息是通过Base64编码添加到HTTP头部中的，但是这种编码并不安全，因此基本认证也不是最安全的认证机制。

为了增强安全性，可以考虑使用其他认证机制，例如Digest认证或OAuth认证。同时，还可以使用HTTPS协议来保护HTTP请求的传输过程中的安全性。

# 参考资料

- [libcurl官方文档](https://curl.haxx.se/libcurl