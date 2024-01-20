---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么与为什么？

发送HTTP请求是一种让你的程序和其他服务器交谈的方式，允许我们通过网络获取和发送数据。为什么程序员需要做这个？因为这是让我们的应用程序与世界互动的重要方式。

## 如何去做：

使用npm的axios库，以下是如何在TypeScript中发送GET和POST请求的示例代码
```TypeScript 
// 引用axios库
import axios from 'axios';

// 发送GET请求
axios.get('https://api.github.com/users/defunkt')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });

// 发送POST请求
axios.post('https://reqres.in/api/users', {
    first_name: 'Pablo',
    last_name: 'Picasso'
  })
  .then(response => {
    console.log(response.data);
  });
```
在这个例子中，我们首先从axios库中导入axios。然后我们分别展示了GET和POST请求的处理方式，同时展示了如何处理成功的响应和失败的错误。

## 深度解析

你可能会考虑其他的HTTP库，比如fetch或者request，但是axios在处理将要发送的请求和接收到的响应时提供了更多的配置选项。它使用的是promise-based API，这使得async/await的使用更为友好。说到历史，axios 是从AngularJS中$q service衍生出来的，而$q service 又是对原生 JavaScript Promises的一个包装。

实现细节方面，Axios在浏览器中处理的是XHRs，而在node.js 中会变换为http。 因此你可以在不同环境中使用同样的代码。此外，你可以轻易地拦截请求和响应，进行转换和取消。

## 另请参阅

1. [Axios GitHub](https://github.com/axios/axios)
2. [MDN HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
3. [TypeScript](https://www.typescriptlang.org/)