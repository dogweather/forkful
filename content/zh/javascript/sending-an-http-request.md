---
title:                "Javascript: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么


在现代的网络世界中，我们经常会使用应用程序来获取和共享信息。想象一下，当你使用社交媒体应用浏览朋友圈，或者在网上购物时，数据是如何从服务器到达你的屏幕上的呢？这里就要用到发送HTTP请求。

## 如何

HTTP（超文本传输协议）是用于客户端和服务器之间通信的标准协议。在Javascript中，我们可以使用内置的XMLHttpRequest对象来发送HTTP请求。

首先，我们需要创建一个XMLHttpRequest对象：

```
let xhr = new XMLHttpRequest();
```

然后，我们需要指定请求的类型、目标URL以及是否以异步方式发送请求：

```
xhr.open('GET', 'https://example.com', true);
```

这里，第一个参数是请求的类型（GET、POST等），第二个参数是请求的URL，第三个参数指定是否以异步方式发送请求（true为异步，false为同步）。

接下来，我们可以添加一些额外的头部信息，比如设置响应类型为json：

```
xhr.responseType = 'json';
```

然后，我们可以发送请求：

```
xhr.send();
```

在请求发送后，我们可以监听请求的状态变化，以便获取响应的数据：

```
xhr.onreadystatechange = function() {
  if (xhr.readyState === XMLHttpRequest.DONE) {
    // 请求完成
    if (xhr.status === 200) {
      // 请求成功，可以使用xhr.response获取响应的数据
    } else {
      // 请求失败
    }
  }
}
```

## 深入了解

HTTP请求的过程实际上是客户端向服务器发送一个请求报文，然后服务器返回一个响应报文。请求报文包含请求的类型、目标URL、请求头部信息和请求体，而响应报文包含状态码、响应头部信息和响应体。

除了使用XMLHttpRequest对象，我们还可以使用轻量级的fetch API来发送HTTP请求。它提供了更简洁的写法，同时支持Promise，让异步操作更加方便。

## 参考链接

- [XMLHttpRequest MDN文档](https://developer.mozilla.org/zh-CN/docs/Web/API/XMLHttpRequest)
- [Fetch API MDN文档](https://developer.mozilla.org/zh-CN/docs/Web/API/Fetch_API)
- [深入理解HTTP协议](https://www.runoob.com/http/http-tutorial.html)

## 详见

- [Node.js文档](https://nodejs.org/api/http.html)
- [Express文档](https://expressjs.com/zh-cn/guide/routing.html)
- [Vue.js文档](https://cn.vuejs.org/v2/guide/ajax.html)