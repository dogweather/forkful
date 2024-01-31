---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:21.189884-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

category:             "C++"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
发送 HTTP 请求就是让你的程序向网站或Web服务请求数据或动作。程序员这么做为了交换数据、获取更新或操纵远程资源。

## How to: (如何操作：)
```C++
#include <iostream>
#include <cpr/cpr.h>

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << "Status code: " << r.status_code << std::endl; // 200
    std::cout << "Response body: " << r.text << std::endl; // JSON response
}
```
输出示例：
```
Status code: 200
Response body: {
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    ...
  },
  "url": "http://httpbin.org/get"
}
```

## Deep Dive (深入探索)
发送 HTTP 请求在网络编程中非常常用。早期C++标准库中没有内建的HTTP支持, 需要依赖如libcurl这样的库。现代而言，C++有了更现代化的库，比如cpr，它是对libcurl的C++封装，使用起来更简单。除cpr外，还可以使用Boost.Beast、Poco等库。在实现上，正确地处理HTTP协议细节、编码、连接管理和超时至关重要。

## See Also (另请参阅)
- cpr GitHub repository: [https://github.com/whoshuu/cpr](https://github.com/whoshuu/cpr)
- libcurl: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- Boost.Beast: [https://www.boost.org/doc/libs/develop/libs/beast/](https://www.boost.org/doc/libs/develop/libs/beast/)
- Poco Project: [https://pocoproject.org/](https://pocoproject.org/)
