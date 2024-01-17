---
title:                "发送 http 请求"
html_title:           "TypeScript: 发送 http 请求"
simple_title:         "发送 http 请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 什么是HTTP请求？为什么程序员会这么做？

当我们在网络上浏览网页或者使用应用程序时，我们所做的每一次点击都会触发一个HTTP请求。发送HTTP请求主要用于获取或发送数据到远程服务器。程序员通常会在他们的应用程序中使用HTTP请求来获取所需的数据，以便于构建用户可以轻松访问的应用程序。

# 如何做？

```TypeScript
import { HttpClient } from '@angular/common/http';

// 创建HttpClient实例
const http = new HttpClient();

// 发送GET请求
http.get('https://www.example.com/data')
  .subscribe(data => console.log(data));

// 发送POST请求
const payload = {
  name: 'John',
  age: 30
};

http.post('https://www.example.com/user', payload)
  .subscribe(result => console.log(result));
```

输出：
```
{ name: 'John', age: 30 }
Success!
```

# 深入了解

HTTP请求是一种客户端-服务器之间的通信方式，它允许程序员通过互联网发送和接收数据。这项技术的诞生可以追溯到1990年代，它是Web 1.0时代的标志性技术之一。除了使用HttpClient，程序员还可以使用XMLHttpRequest对象来发起HTTP请求。不过，最近几年，Web开发越来越流行的框架和库（如Angular、React和Vue）都提供了方便的API来处理HTTP请求，使得发送HTTP请求变得更加容易。

# 查看更多

- 关于HTTP请求的更多信息，请查看MDN文档：https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- Angular中的HttpClient文档：https://angular.io/guide/http