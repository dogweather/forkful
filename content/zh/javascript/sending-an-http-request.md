---
title:                "发出 HTTP 请求"
date:                  2024-01-20T18:00:06.671907-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在JavaScript中发送HTTP请求是与服务器交换数据的方式。程序员这么做主要是为了从服务器获取数据或向服务器发送数据，动态更新网页内容而不需要重新加载页面。

## How to: (如何操作：)
```Javascript
// 使用fetch API发送GET请求
fetch('https://api.example.com/data')
  .then(response => response.json()) // 解析JSON响应
  .then(data => console.log(data))   // 使用获得的数据
  .catch(error => console.error('Error:', error));

// 使用fetch API发送POST请求
fetch('https://api.example.com/data', {
  method: 'POST', // 指定请求方法
  headers: {
    'Content-Type': 'application/json', // 设置请求头
  },
  body: JSON.stringify({ key: 'value' }) // 发送的数据
})
  .then(response => response.json())
  .then(data => console.log('Success:', data))
  .catch(error => console.error('Error:', error));
```
运行这些代码片段，你会看到控制台输出获取或发送数据的结果。

## Deep Dive (深入探究)
早期，XMLHttpRequest是实现AJAX通信的方式，允许从页面上异步发送请求。现在，fetch API因为其基于Promise的设计而更受欢迎，它提供了一种更简洁、现代的方法来进行网络请求。尽管有这些选择，某些老旧浏览器可能需要降级使用XMLHttpRequest。实现细节方面，浏览器为JavaScript提供了全局`fetch`函数，它支持CORS、可以发送各种类型的请求并且可以很容易地处理响应。

## See Also (另请参阅)
- MDN Web Docs上的fetch API: [MDN Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- XMLHttpRequest文档: [MDN XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- HTTP请求方法: [MDN HTTP Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- JSON.stringify的细节: [MDN JSON.stringify](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)
- 错误处理和Promises: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
