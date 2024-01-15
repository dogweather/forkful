---
title:                "使用基本认证发送http请求"
html_title:           "TypeScript: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

基本认证是一种通过在HTTP请求中包括用户名和密码来验证用户身份的方式。它可以帮助我们保护敏感数据和资源，以及限制未经授权的访问。

## 如何执行HTTP请求和基本认证

首先，我们需要安装`axios`包来处理HTTP请求，可以通过以下命令进行安装：

```
npm install axios
```

接下来，我们需要导入`axios`包，并创建一个实例来执行我们的HTTP请求，在代码中添加以下行：

```
const axios = require('axios');

const instance = axios.create({
  baseURL: 'https://example.com', // 替换为你想要发送请求的URL
  auth: {
    username: 'username', // 替换为你的用户名
    password: 'password' // 替换为你的密码
  }
});
```

现在，我们可以使用`instance`来发送我们的HTTP请求。例如，我们可以发送一个GET请求来获取页面内容，并在控制台输出结果。在代码中，我们可以添加以下行：

```
instance.get('/page')
  .then(function (response) {
    console.log(response.data);
  })
  .catch(function (error) {
    console.log(error);
  });
```

在上述示例中，我们使用`then`和`catch`方法来处理请求的响应和错误。你可以根据自己的需求来使用不同的HTTP方法（GET、POST、PUT等）以及适当的响应处理。

## 深入了解

在发送HTTP请求时，我们可以通过在请求头中添加`Authorization`字段来实现基本认证。这可以通过将用户名和密码使用Base64编码来创建一个令牌来实现。在上面的例子中，我们使用`auth`选项来通过提供用户名和密码来方便地添加`Authorization`头部。

此外，我们还可以使用`axios`提供的拦截器（interceptors）来进行更多的自定义。使用拦截器，我们可以在每个请求中添加特定的头部，以及在响应时验证和处理错误。你可以通过阅读[axios的官方文档](https://github.com/axios/axios)来了解更多关于拦截器的信息。

## 参考链接

- [axios官方文档](https://github.com/axios/axios)
- [理解HTTP基本认证](https://www.ibm.com/docs/en/datapower-gateways/2018.4?topic=topics-http-authentication)