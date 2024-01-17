---
title:                "通过基本身份验证发送一个 http 请求"
html_title:           "C++: 通过基本身份验证发送一个 http 请求"
simple_title:         "通过基本身份验证发送一个 http 请求"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 啥是HTTP请求与基本认证？为什么程序员会用它？

HTTP请求是一个在客户端设备（如电脑）和服务器之间进行通信的方法，它允许程序员发送和接收数据，以便进行网络通信。基本认证是一种简单的身份验证方法，通过要求用户提供用户名和密码来确认用户的身份。程序员通常会使用这种认证方式来访问受保护的网络资源或API，以确保数据的安全性。

# 怎样做？

以下示例展示了如何发送带有基本认证的HTTP请求，你需要使用C++编程语言：

```C++
#include <iostream>  
#include <curl/curl.h> //需要先安装cURL库

using namespace std;

int main()
{
  //设置用户名和密码，这些信息将在HTTP请求的标头中使用
  const char* username = "用户名";
  const char* password = "密码";

  //初始化cURL句柄
  CURL *curl = curl_easy_init();
  if (curl) {
    //设置请求的URL地址
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
    //设置认证方式为基本认证
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    //设置用户名和密码
    curl_easy_setopt(curl, CURLOPT_USERNAME, username);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, password);
    //发送请求
    CURLcode res = curl_easy_perform(curl);
    //检查请求是否成功
    if (res == CURLE_OK) {
      cout << "请求已发送成功" << endl;
    } else {
      cout << "请求失败: " << curl_easy_strerror(res) << endl;
    }
    //清除cURL句柄
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

输出：

```bash
请求已发送成功
```

# 深入了解

基本认证是最古老的身份验证机制之一，它最初是为了使Internet上的各种网络资源受到保护而设计的。它是一种简单有效的方法，但也有一些缺点，比如无法加密发送的信息。因此，现在许多人更倾向于使用更安全的认证方式，比如OAuth。

除了基本认证，还有其他一些认证方式，比如摘要认证、OAuth、API密钥等。每种方式都有自己的优缺点，程序员应根据具体情况来选择最合适的认证方式。

HTTP请求中的基本认证步骤其实还有更多细节，比如设置响应状态码和处理错误信息。如果想要进一步了解更多细节，可以查阅cURL库的文档或者搜索相关信息。

# 参考链接

- cURL库文档：https://curl.haxx.se/libcurl/c/
- cURL Github仓库：https://github.com/curl/curl
- 各种HTTP认证方式的比较：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication#Comparison_table_of_different_techniques