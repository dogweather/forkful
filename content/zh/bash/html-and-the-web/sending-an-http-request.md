---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:58:58.994715-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? 为什么以及为何？
发送HTTP请求就是让你的程序通过网络与另一台计算机的服务进行通信。程序员这么做主要是为了获取数据、发送数据、或与远程服务进行交互。

## How to: 如何操作
在Bash里，你可以用几个工具来发送HTTP请求，最常见的是`curl`。这里有个简单例子：

```Bash
# 发送GET请求
curl http://example.com

# 发送带数据的POST请求
curl -d "param1=value1&param2=value2" -X POST http://example.com/resource

# 设置请求头部
curl -H "Content-Type: application/json" -X GET http://example.com
```

运行后的输出将直接显示请求结果。

## Deep Dive 深入探讨
HTTP请求的原理贯穿了现代网络通讯的大部分。最初的HTTP/1.1协议在1997年定义，为我们提供了结构化数据交换的能力。Bash中发送HTTP请求的其他工具包括`wget`和面向更复杂场景的`httpie`。而现在，我们甚至有了HTTP/2和HTTP/3来优化性能。对于Bash中发送请求的细致实现，`curl`提供了广泛的参数来控制请求的各个方面，比如超时、http方法、http版本等。

## See Also 另请参阅
- [curl官方网站](https://curl.se/)
- [HTTP客户端httpie](https://httpie.io/)
- [维基百科 - 超文本传输协议](https://zh.wikipedia.org/wiki/超文本传输协议)
