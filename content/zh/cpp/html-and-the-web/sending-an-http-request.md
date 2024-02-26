---
date: 2024-01-20 17:59:21.189884-07:00
description: "\u53D1\u9001 HTTP \u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\
  \u5411\u7F51\u7AD9\u6216Web\u670D\u52A1\u8BF7\u6C42\u6570\u636E\u6216\u52A8\u4F5C\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\u4E86\u4EA4\u6362\u6570\u636E\u3001\
  \u83B7\u53D6\u66F4\u65B0\u6216\u64CD\u7EB5\u8FDC\u7A0B\u8D44\u6E90\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.673543-07:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001 HTTP \u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\
  \u5411\u7F51\u7AD9\u6216Web\u670D\u52A1\u8BF7\u6C42\u6570\u636E\u6216\u52A8\u4F5C\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\u4E86\u4EA4\u6362\u6570\u636E\u3001\
  \u83B7\u53D6\u66F4\u65B0\u6216\u64CD\u7EB5\u8FDC\u7A0B\u8D44\u6E90\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
