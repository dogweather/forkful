---
date: 2024-01-20 17:59:21.189884-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u8F93\u51FA\u793A\u4F8B\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.400988-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
