---
title:                "发送HTTP请求"
html_title:           "Javascript: 发送HTTP请求"
simple_title:         "发送HTTP请求"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么要发送HTTP请求

发送HTTP请求是与服务器通信的主要方式。它允许我们获取、发送或更新数据，从而构建交互式的网络应用程序。

## 如何发送HTTP请求

发送HTTP请求的方法有很多，但是最常用的是使用Javascript中内置的XMLHttpRequest对象。以下是一个简单的示例代码：

```Javascript
// 创建XMLHttpRequest对象
const xhr = new XMLHttpRequest();

// 指定请求目标和请求类型
xhr.open('GET', 'https://api.example.com/users');

// 发送请求
xhr.send();

// 监听请求状态改变事件
xhr.onreadystatechange = function() {
  // 检查请求状态
  if (xhr.readyState === XMLHttpRequest.DONE) {
    // 检查响应状态
    if (xhr.status === 200) {
      // 响应成功，使用responseText获取响应数据
      const data = xhr.responseText;
      console.log(data);
    } else {
      // 响应失败，输出错误信息
      console.log("Error: " + xhr.status);
    }
  }
}
```

以上代码会向"https://api.example.com/users"发送一个GET请求，并在控制台输出响应数据。

## 深入探讨HTTP请求

除了上面提到的XMLHttpRequest对象，还有其他方法可以发送HTTP请求，比如使用第三方库如Axios、Fetch等，或者使用浏览器中内置的Fetch API。

HTTP请求的一个重要组成部分是URL，它由以下几部分组成：

- 协议：一般为“http”或“https”
- 域名：服务器的地址，比如“api.example.com”
- 端口（可选）：服务器的HTTP端口，默认为80，HTTPS端口为443
- 路径（可选）：指定请求的资源路径，比如“/users”
- 查询参数（可选）：使用问号“?”将参数和值连接，多个参数用“&”分隔，比如“?id=123&name=john”

除了GET请求外，还有POST、PUT、DELETE等其他类型的HTTP请求，它们可以在请求中传递不同类型的数据，如表单数据、JSON数据等。

## 查看也可以

- [XMLHttpRequest对象文档](https://developer.mozilla.org/zh-CN/docs/Web/API/XMLHttpRequest)
- [Fetch API文档](https://developer.mozilla.org/zh-CN/docs/Web/API/Fetch_API)
- [Axios文档](https://github.com/axios/axios)