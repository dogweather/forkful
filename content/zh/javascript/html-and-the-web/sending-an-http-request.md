---
date: 2024-01-20 18:00:06.671907-07:00
description: "\u5728JavaScript\u4E2D\u53D1\u9001HTTP\u8BF7\u6C42\u662F\u4E0E\u670D\
  \u52A1\u5668\u4EA4\u6362\u6570\u636E\u7684\u65B9\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u4ECE\u670D\u52A1\u5668\u83B7\u53D6\u6570\
  \u636E\u6216\u5411\u670D\u52A1\u5668\u53D1\u9001\u6570\u636E\uFF0C\u52A8\u6001\u66F4\
  \u65B0\u7F51\u9875\u5185\u5BB9\u800C\u4E0D\u9700\u8981\u91CD\u65B0\u52A0\u8F7D\u9875\
  \u9762\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.263865
model: gpt-4-1106-preview
summary: "\u5728JavaScript\u4E2D\u53D1\u9001HTTP\u8BF7\u6C42\u662F\u4E0E\u670D\u52A1\
  \u5668\u4EA4\u6362\u6570\u636E\u7684\u65B9\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u4ECE\u670D\u52A1\u5668\u83B7\u53D6\u6570\u636E\
  \u6216\u5411\u670D\u52A1\u5668\u53D1\u9001\u6570\u636E\uFF0C\u52A8\u6001\u66F4\u65B0\
  \u7F51\u9875\u5185\u5BB9\u800C\u4E0D\u9700\u8981\u91CD\u65B0\u52A0\u8F7D\u9875\u9762\
  \u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
