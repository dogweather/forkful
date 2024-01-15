---
title:                "使用基础认证发送http请求"
html_title:           "Javascript: 使用基础认证发送http请求"
simple_title:         "使用基础认证发送http请求"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

发送带有基本认证的HTTP请求可能是非常有用的，特别是在需要对访问进行认证的情况下。例如，当您需要从受保护的API中获取数据时，基本认证允许您通过提供用户名和密码来验证您的身份，从而获得对API的访问权限。

## 如何操作

```Javascript 
const url = 'https://api.example.com/data'; 
const username = 'myusername'; 
const password = 'mypassword';

// 设置基本认证凭证
const credentials = btoa(username + ':' + password);

// 构建HTTP请求
const request = new XMLHttpRequest();

// 设置请求方法和API URL
request.open('GET', url);

// 添加Authorization头
request.setRequestHeader('Authorization', 'Basic ' + credentials);

// 发送请求
request.send();

// 处理响应
request.onload = function() {
  if (request.status === 200) {
    console.log(request.responseText);
    // 输出结果如：{"id": 123, "name": "John"}
  } else {
    console.log('请求失败');
  }
}
```

## 深入了解

在发送HTTP请求时，如果需要对访问进行认证，使用基本认证可以是一种有效的方式。基本认证是一种简单的用户身份验证方法，它要求用户提供用户名和密码作为凭证，然后将凭证进行Base64编码后添加到请求头的Authorization字段中。在服务器端，接收到请求后会解码凭证并验证用户名和密码是否正确，如果正确则返回请求所需的数据。如果项目中需要更加安全的认证方法，可以考虑使用OAuth等其他方式。

## 看看这些

- [XMLHttpRequest API介绍](https://developer.mozilla.org/zh-CN/docs/Web/API/XMLHttpRequest)
- [Base64编码解码](https://developer.mozilla.org/zh-CN/docs/Web/API/WindowBase64/Base64_encoding_and_decoding)
- [HTTP基本认证详解](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)