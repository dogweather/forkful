---
title:                "TypeScript: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

在现代的互联网世界中，我们经常需要与远程服务器交互，从而获取所需的数据。为了保护服务器免受未经授权的访问，我们需要发送HTTP请求时进行身份验证。基本身份验证是一种简单但常用的身份验证方法，它可以通过发送用户名和密码来验证用户的身份，并允许用户访问受保护的资源。

## 如何

要发送带有基本身份验证的HTTP请求，我们可以使用TypeScript中的内置模块“http”或一些流行的HTTP客户端库，如“axios”或“node-fetch”。让我们来看看如何使用这些方法来发送请求：

```
// 使用内置模块http发送HTTP请求
import * as http from "http";

// 设置请求地址
const url = "http://www.example.com/api/users";

// 设置身份验证凭据
const username = "user";
const password = "pass";

// 设置请求选项
const options = {
  auth: `${username}:${password}` // 使用用户名和密码进行基本身份验证
};

// 发送HTTP GET请求
http.get(url, options, (response) => {
  // 处理响应
  response.on("data", (data) => {
    const result = data.toString();
    console.log(result);
  });
});
```

下面是使用“axios”库发送HTTP请求的示例：

```
// 安装axios库
npm install axios --save

// 导入axios
import axios from "axios";

// 设置请求地址
const url = "http://www.example.com/api/users";

// 设置身份验证凭据
const username = "user";
const password = "pass";

// 发送HTTP GET请求
axios.get(url, {
  auth: {
    username: username,
    password: password
  }
})
.then(response => {
  // 处理响应
  const result = response.data;
  console.log(result);
})
.catch(error => {
  // 处理错误
  console.log(error);
});
```

无论使用哪个方法，我们都可以通过检查响应的状态码来判断身份验证是否成功。401状态码表示身份验证失败，而200表示成功。

## 深入了解

基本身份验证并不是最安全的身份验证方法，因为它只是通过简单地传递用户名和密码来验证用户的身份。因此，我们建议只在HTTPS协议下使用它。另外，为了进一步保护用户的身份，我们也可以使用更复杂的身份验证方法，如OAuth。

## 参考链接

- Node.js官方文档：https://nodejs.org/api/http.html#http_http_get_url_options_callback
- Axios库文档：https://axios-http.com/docs/intro
- Node-fetch库文档：https://www.npmjs.com/package/node-fetch