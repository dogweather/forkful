---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
发送包含基本认证的HTTP请求是一种常用的网络编程操作。程序员之所以要这么做，是因为它让我们可以向需要用户名和密码的网址发起请求。

## 如何：
在C语言中，我们常常用libcurl库来处理这类问题。以下是一个简单的代码示例：

```C
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();

    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        /* set username and password for basic authentication */
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "passwd");

        /* Perform the request */
        res = curl_easy_perform(curl);
        
        /* Check for errors */ 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();
    return 0;
}
```

请注意，你要确认是否已经在你的电脑上安装了libcurl库并且包含在你的编译器路径里。

## 深入：
发送HTTP请求是web编程的核心部分。由于不同的编程语言和库有各自的实现方式，我们选择C语言和libcurl库，是因为它简洁直观，更加适合引导初学者了解这个问题。实际上，还有一些其他方式来发送HTTP请求，例如使用socket编程、或者其他的第三方库如libwww。

从实现细节来说，基本认证将用户名和密码组合成一个字符串：“username:password”，然后用Base64算法编码。最后，将这个编码后的字符串添加到HTTP请求头部：“Authorization: Basic {编码后的字符串}”。

## 相关链接： 
1. libcurl官方文档：https://curl.se/libcurl/c/
2. HTTP认证详解：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication
3. libwww库：https://www.w3.org/Library/
4. Socket编程基础：http://beej.us/guide/bgnet/html/single/bgnet.html 

希望这篇文章能够帮到你理解在C语言中如何发送带有基本认证的HTTP请求。欢迎继续学习！