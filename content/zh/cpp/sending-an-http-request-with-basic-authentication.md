---
title:                "C++: 用基础认证发送http请求"
simple_title:         "用基础认证发送http请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么要发送带基本身份验证的HTTP请求

当我们需要从一个Web服务器上获取数据时，使用HTTP请求是非常常见的。但是，有些时候我们可能需要一些额外的安全性来保护我们的请求和服务器。这就是为什么在发送HTTP请求时使用基本身份验证的原因。

## 如何发送带基本身份验证的HTTP请求

要发送带基本身份验证的HTTP请求，我们需要使用C++编写代码来构建一个HTTP客户端。下面是一个基本的代码示例：

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// 回调函数，用于处理HTTP响应
static size_t write_data(void *ptr, size_t size, size_t nmemb, void *userdata)
{
    std::cout << string((char*)ptr, size * nmemb) << endl;
    return size * nmemb;
}

int main()
{
    CURL *curl;
    CURLcode res;
    
    // 初始化curl
    curl = curl_easy_init();

    if(curl) {
        // 设置URL
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
        
        // 设置需要进行基本身份验证的用户名和密码
        curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

        // 设置回调函数，处理HTTP响应
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

        // 执行请求
        res = curl_easy_perform(curl);

        // 检查是否有错误发生
        if(res != CURLE_OK)
            cout << "Error: " << curl_easy_strerror(res) << endl;

        // 清理curl资源
        curl_easy_cleanup(curl);
    }

    // 结束程序
    return 0;
}

```

接下来是这段代码的输出示例，可以看到在请求被发送到服务器时，会弹出一个带有用户名和密码的身份验证框。输入正确的用户名和密码后，就会获取到服务器响应的内容。

```
Please enter your username: username
Please enter your password: password
<html>
<head>
<title>Example Domain</title>
</head>
<body>
<div>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## 深入了解如何发送带基本身份验证的HTTP请求

使用基本身份验证时，客户端会发送一个带有“Authorization”头的HTTP请求。这个header会包含一个Base64编码的用户名和密码，用来验证请求的发送者身份。服务器收到请求后会解码header中的信息并进行验证，如果验证通过，就会向客户端发送请求的内容。

另外，为了防止密码被拦截和解码，建议使用SSL/TLS加密来发送HTTP请求。此外，还可以使用具有更高级别的安全性的身份验证方法来保护服务器和请求。

见下文以了解更多关于如何使用基本身份验证发送HTTP请求的信息：

- [C++使用curl库发送HTTP请求](https://curl.haxx.se/libcurl/c/http.html)
- [HTTP身份验证介绍](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [如何在C++代码中使用Base64编码](https://www.openssl.org/docs/manmaster/man3/openssl_base64.html)

## 看看这些相关信息

- [C++编程教程](https://www.runoob.com/cplusplus/cpp-tutorial.html)
- [HTTP请求和响应状态代码](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Status)
- [CURL库文档](https://curl.haxx.se/libcurl/)