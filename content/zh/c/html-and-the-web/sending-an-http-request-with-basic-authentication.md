---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:01.117926-07:00
description: "\u5982\u4F55\u6267\u884C\uFF1A \u8981\u5728 C \u8BED\u8A00\u4E2D\u53D1\
  \u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684 HTTP \u8BF7\u6C42\uFF0C\u6211\u4EEC\
  \u9700\u8981\u4F7F\u7528 libcurl \u5E93\uFF0C\u8FD9\u662F\u4E00\u4E2A\u6D41\u884C\
  \u7684\u3001\u591A\u529F\u80FD\u7684\u3001\u6613\u4E8E\u4F7F\u7528\u7684\u5BA2\u6237\
  \u7AEF URL \u4F20\u8F93\u5E93\u3002\u5B83\u5904\u7406\u5404\u79CD\u534F\u8BAE\uFF0C\
  \u5305\u62EC HTTP \u548C HTTPS\uFF0C\u4F7F\u6211\u4EEC\u7684\u4EFB\u52A1\u53D8\u5F97\
  \u66F4\u7B80\u5355\u3002\u5728\u7EE7\u7EED\u4E4B\u524D\uFF0C\u8BF7\u786E\u4FDD\u60A8\
  \u7684\u7CFB\u7EDF\u4E2D\u5B89\u88C5\u4E86\u2026"
lastmod: '2024-04-05T22:38:47.456408-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u6267\u884C\uFF1A \u8981\u5728 C \u8BED\u8A00\u4E2D\u53D1\u9001\
  \u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684 HTTP \u8BF7\u6C42\uFF0C\u6211\u4EEC\u9700\
  \u8981\u4F7F\u7528 libcurl \u5E93\uFF0C\u8FD9\u662F\u4E00\u4E2A\u6D41\u884C\u7684\
  \u3001\u591A\u529F\u80FD\u7684\u3001\u6613\u4E8E\u4F7F\u7528\u7684\u5BA2\u6237\u7AEF\
  \ URL \u4F20\u8F93\u5E93\u3002\u5B83\u5904\u7406\u5404\u79CD\u534F\u8BAE\uFF0C\u5305\
  \u62EC HTTP \u548C HTTPS\uFF0C\u4F7F\u6211\u4EEC\u7684\u4EFB\u52A1\u53D8\u5F97\u66F4\
  \u7B80\u5355\u3002\u5728\u7EE7\u7EED\u4E4B\u524D\uFF0C\u8BF7\u786E\u4FDD\u60A8\u7684\
  \u7CFB\u7EDF\u4E2D\u5B89\u88C5\u4E86 libcurl\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\
  \u57FA\u672C\u793A\u4F8B\uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u53D1\u9001\u5E26\u6709\
  \u57FA\u672C\u8BA4\u8BC1\u7684 GET \u8BF7\u6C42\uFF1A."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001HTTP\u8BF7\u6C42"
weight: 45
---

## 如何执行：
要在 C 语言中发送带有基本认证的 HTTP 请求，我们需要使用 libcurl 库，这是一个流行的、多功能的、易于使用的客户端 URL 传输库。它处理各种协议，包括 HTTP 和 HTTPS，使我们的任务变得更简单。在继续之前，请确保您的系统中安装了 libcurl。这里有一个基本示例，展示了如何发送带有基本认证的 GET 请求：

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // 正在向其发送请求的 URL
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // 启用基本认证的使用
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // 提供基本认证的用户名和密码
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // 执行 GET 请求
        res = curl_easy_perform(curl);

        // 检查错误
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // 总是清理
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
在上面的示例中，将 `"http://example.com/resource"`、`"username"` 和 `"password"` 替换为您实际的 URL、用户名和密码。

这段代码初始化了一个 `CURL` 对象，设置了 URL、启用了 HTTP 基本认证，并指定了凭证。然后它发送请求并在完成后进行清理。如果成功，将获取请求的资源；如果有错误，将打印到 stderr。

示例输出（假设成功进行了认证和资源访问）可能不会由程序直接显示，因为该示例主要演示发送请求。要打印响应，您会扩展程序以处理 HTTP 响应数据。

## 深入了解：
如所示，在 C 中发送带有基本认证的 HTTP 请求利用了 libcurl 库的健壮性和简单性。历史上，仅使用 C 语言而不借助这类库来制作 HTTP 请求是麻烦且容易出错的，涉及较低级别的套接字编程和手动构造 HTTP 头部。

基本认证本身是 Web 早期的一种方法。它以容易解码的格式（Base64）发送凭证，这在明文通道上本质上是不安全的。现代应用程序通常更倾向于使用更安全的认证方法，如 OAuth 2.0 或 JWT（JSON Web 令牌），特别是对于敏感数据。

然而，对于内部的、不那么关键的系统，或快速而简单的脚本，其中便利性超过了安全性顾虑时，基本认证仍在使用。此外，当与加密连接（HTTPS）结合时，其简单性在快速开发、测试或自动化工作中成为优势，这些场合可能不需要更高级别的安全机制。

在安全性至关重要的情况下，应优先考虑如基于令牌的认证等替代方法。尽管如此，通过 libcurl 理解如何在 C 中实现基本认证提供了一项基础技能，可以适用于各种认证方法和协议，反映了在 web 开发中安全性、便利性和应用需求之间的细微权衡。
