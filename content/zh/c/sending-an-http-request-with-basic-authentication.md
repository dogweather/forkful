---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:01:12.455833-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (什麼 & 為什麼？)

发送带基本认证的HTTP请求，是为了访问需要用户名和密码的网络资源。程序员这样做保障通信安全，限制资源的访问。

## How to: (如何做：)

以下是一个C语言示例，演示如何使用libcurl库发送带有基本认证的HTTP请求。

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

        res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

Sample output （样例输出）:
```
curl_easy_perform() failed: Failed to connect to example.com port 80: Connection refused
```
注意：请替换实际的网址、用户名和密码。

## Deep Dive (深入探究):

发送带基本认证的HTTP请求跟互联网初期web认证机制相关。基本认证（Basic Authentication）在1990年代初定义，用来简单快速地控制资源。虽然有更安全的认证方法（比如OAuth2、JWT），很多API和服务还是支持基本认证，尤其是内部或受限访问场景。

关于实现细节，基本认证通过HTTP头 `Authorization` 发送，内容是 'Basic ' 加上用户名和密码的Base64编码。不过，直接处理HTTP请求和认证细节复杂，容易出错。因此，使用像libcurl这样的库可以简化操作，自动处理编码和连接细节。

## See Also (另请参阅):

- [The libcurl tutorial](https://curl.se/libcurl/c/libcurl-tutorial.html)
- [Basic access authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
