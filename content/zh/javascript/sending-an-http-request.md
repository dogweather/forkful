---
title:                "发送一次http请求"
html_title:           "Javascript: 发送一次http请求"
simple_title:         "发送一次http请求"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

什么是发送HTTP请求，以及为什么程序员这样做？
发送HTTP请求是将信息从一个程序发送到另一个程序的步骤。程序员这样做是为了与其他程序进行通信，从而获得所需的信息。

怎样做：
```Javascript
// 使用XMLHttpRequest对象发送HTTP请求
var xhr = new XMLHttpRequest();
xhr.open('GET', 'http://www.example.com/', true); // 在第一个参数中指定请求方法，第二个参数是请求的URL，第三个参数表示该请求是否应该异步处理。
xhr.send(); // 发送请求
console.log(xhr.responseText); // 获取服务器的响应
```
输出：如果成功发送了请求，控制台会打印出服务器返回的文本。

深入探讨：
发送HTTP请求的历史背景：在Web发展初期，网页只能通过链接来进行通信，但是这种方式有限制。随着技术的发展，出现了更加高效和灵活的HTTP请求方式，从而推动了Web的发展。
其他可选的方法：除了使用XMLHttpRequest对象，还可以使用Fetch API来发送HTTP请求。这是一种基于Promise的新技术，使得请求更加简洁和易于使用。
实现细节：程序员可根据自己的需求使用不同的HTTP请求方法，如GET、POST、PUT和DELETE。通常，服务器会返回一个状态码来表示请求的成功或失败。

也可以参考：
- MDN的XMLHttpRequest文档：https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest
- MDN的Fetch API文档：https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API