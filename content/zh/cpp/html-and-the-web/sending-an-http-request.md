---
date: 2024-01-20 17:59:21.189884-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u53D1\u9001 HTTP \u8BF7\u6C42\
  \u5728\u7F51\u7EDC\u7F16\u7A0B\u4E2D\u975E\u5E38\u5E38\u7528\u3002\u65E9\u671FC++\u6807\
  \u51C6\u5E93\u4E2D\u6CA1\u6709\u5185\u5EFA\u7684HTTP\u652F\u6301,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.315695-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u53D1\u9001 HTTP \u8BF7\u6C42\u5728\u7F51\
  \u7EDC\u7F16\u7A0B\u4E2D\u975E\u5E38\u5E38\u7528\u3002\u65E9\u671FC++\u6807\u51C6\
  \u5E93\u4E2D\u6CA1\u6709\u5185\u5EFA\u7684HTTP\u652F\u6301, \u9700\u8981\u4F9D\u8D56\
  \u5982libcurl\u8FD9\u6837\u7684\u5E93\u3002\u73B0\u4EE3\u800C\u8A00\uFF0CC++\u6709\
  \u4E86\u66F4\u73B0\u4EE3\u5316\u7684\u5E93\uFF0C\u6BD4\u5982cpr\uFF0C\u5B83\u662F\
  \u5BF9libcurl\u7684C++\u5C01\u88C5\uFF0C\u4F7F\u7528\u8D77\u6765\u66F4\u7B80\u5355\
  \u3002\u9664cpr\u5916\uFF0C\u8FD8\u53EF\u4EE5\u4F7F\u7528Boost.Beast\u3001Poco\u7B49\
  \u5E93\u3002\u5728\u5B9E\u73B0\u4E0A\uFF0C\u6B63\u786E\u5730\u5904\u7406HTTP\u534F\
  \u8BAE\u7EC6\u8282\u3001\u7F16\u7801\u3001\u8FDE\u63A5\u7BA1\u7406\u548C\u8D85\u65F6\
  \u81F3\u5173\u91CD\u8981\u3002"
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
