---
title:                "Bash: 通过基本认证发送http请求"
simple_title:         "通过基本认证发送http请求"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

为什么要使用基本身份验证来发送HTTP请求？基本身份验证是一种常用的网络身份验证方法，它可以帮助确保我们在发送数据时的安全性和隐私保护。通过在HTTP请求中添加基本身份验证信息，服务器可以识别用户身份并验证其合法性，确保只有经过授权的用户才能访问受保护的资源。

## 如何使用Bash发送带基本身份认证的HTTP请求

对于那些想要使用Bash来发送HTTP请求并使用基本身份认证的用户，我们提供以下示例代码及其对应的输出：

```Bash
# 发送GET请求到指定URL，并使用基本身份认证
curl -u username:password https://www.example.com

# 发送POST请求并同时添加数据，然后使用基本身份认证
curl -u username:password -d "name=john&age=25" https://www.example.com

# 显示HTTP请求头信息
curl -u username:password -I https://www.example.com

# 下载文件并保存到本地目录，同时使用基本身份认证
curl -u username:password -O https://www.example.com/example.pdf
```

输出示例：

```
# GET 请求的输出
<html>
<head>
<title>Welcome to Example.com</title>
</head>
<body>
<h1>Hello, World!</h1>
</body>
</html>

# POST 请求的输出
{"status": "success"}

# 头信息的输出
HTTP/1.1 200 OK
Date: Sat, 20 Mar 2021 08:00:00 GMT
Content-Type: text/html; charset=utf-8
Content-Length: 1024
# 等等...
```

## 深入学习

发送带基本身份认证的HTTP请求的过程实际上是将一个含有身份信息的`Authorization`头部添加到HTTP请求中。这个`Authorization`头部通常包括用户名和密码的Base64编码后的字符串。例如，如果用户名为"username"，密码为"12345"，那么将其转换为Base64编码后得到的字符串就是"username:12345"，然后将这个字符串添加到`Authorization`头部即可。但是，为了进一步提高安全性，我们还可以在用户名和密码之间添加一个“:”符号，然后再进行Base64编码，这样在传输过程中就难以被窃取。

另外，如果需要在Bash中使用加密的用户名和密码来发送基本身份认证的HTTP请求，我们也可以使用`-u`选项后面跟随加密字符串的方式来实现。例如，我们可以使用`-u dXNlcm5hbWU6cGFzc3dvcmQ=`来替代`-u username:password`。

## 参考链接

- [curl官方文档](https://curl.se/docs/)
- [基本身份认证（Basic Authentication）简介](https://www.runoob.com/http/http-basic-authentication.html)
- [Base64编码简介](https://zh.wikipedia.org/wiki/Base64)
- [curl命令使用详解](https://www.runoob.com/linux/linux-comm-curl.html)

# 请参考

- [curl命令基础教程（中文）](https://www.cnblogs.com/djwxzy/p/12429760.html)
- [基本身份验证（Basic Authentication）的使用及其安全性分析](https://blog.csdn.net/xiaoersoftware/article/details/80484322)
- [如何在Bash中使用加密的用户名和密码进行HTTP请求](https://stackoverflow.com/questions/48613444/http-basic-authentication-with-bash-curl)