---
title:                "发送一个http请求"
html_title:           "Bash: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

发送HTTP请求是一种程序员用来与网络服务器进行通信的方式。程序员通常会通过发送HTTP请求来获取网页的信息，以便在其编写的应用程序中使用。

## 如何：

### 发送GET请求：
```Bash
curl http://example.com
```

输出示例：
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>
<body>
<div>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
  <p style="float:right;">More information...</p>
</div>
</body>
</html>
```

### 发送POST请求：
```Bash
curl -X POST -d '{"username":"john","password":"12345"}' http://example.com/login
```

输出示例：
```
{"status":"success","message":"Login successful"}
```

## 深入了解：

### 历史背景：
HTTP协议是一种用于网络通信的协议，于1991年被提出并广泛应用于互联网。最初，HTTP请求是通过telnet命令来发送的，直到1996年，Netscape Navigator发布了一款支持图形界面的浏览器，使得HTTP请求变得更加简单和方便。

### 其他选择：
除了使用Bash命令来发送HTTP请求，程序员也可以使用其他工具，比如Postman和cURL库。

### 实现细节：
Bash提供了一系列命令来发送HTTP请求，包括curl、wget和httpie。这些命令都支持各种请求方法，如GET、POST和PUT，并可以通过设置不同的参数来控制请求的行为。

## 查看相关资料：

- <https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods>
- <https://github.com/curl/curl>
- <https://httpie.org/>
- <https://www.postman.com/>