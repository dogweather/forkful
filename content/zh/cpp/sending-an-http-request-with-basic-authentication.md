---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 是什么以及为什么?

发送带有基础认证的HTTP请求是一种常见的网络请求方式，其中将用户ID和密码以 Base64 编码后的形式携带在请求头中。编程人员通过这种方式满足在服务器上进行身份验证以访问受保护的资源。

## 如何实现：

我们可以使用模块上的网络请求库来发送 HTTP 请求。以下是一个在C++中发送带有基本身份验证的HTTP请求的简单示例。我们在这个例子中使用了 `cpr` 库。

```C++
// 包含请求库
#include <cpr/cpr.h>

int main(){
    // 创建认证对
    cpr::Authentication auth{"username", "password"};

    // 发送GET请求
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/basic-auth/username/password"}, auth);

    // 打印响应 
    std::cout << r.text << std::endl;

    return 0;
}
```

程序的输出可能 resemble以下形式：

```JSON
{
  "authenticated": true,
  "user": "username"
}
```

##深入研究：

1. 历史背景：最初，HTTP Basic认证由RFC 1945（HTTP / 1.0规范）定义，而后由RFC 2617定义。然而，该方法的主要问题是安全性较低，因为它将用户名和密码明文（尽管是Base64编码）放在HTTP头中。
2. 替代方案：因为上述的安全问题，程序员可能选择其他认证方法，例如使用OAuth，Bearer tokens，或者是Digest authentication。  
3. 实施细节：在处理带认证的HTTP请求时，一旦服务器接收到请求，它会解析并验证包含的凭据。如果凭据无效或缺失，服务器将返回状态码401，提示客户端需要提供有效的认证凭据。

## 另请参阅：

1. [CPR库](https://whoshuu.github.io/cpr/)：C ++的Curl for People，一个轻量且易于使用的网络库。
2. [HTTP Basic, Digest, NTLM 认证](https://en.wikipedia.org/wiki/Basic_access_authentication)：深度理解基本认证及其替代方案。
3. [RFC 7617](https://tools.ietf.org/html/rfc7617)：HTTP Basic and Digest access authentication的标准文档。