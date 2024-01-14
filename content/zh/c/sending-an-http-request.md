---
title:                "C: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP请求为什么重要
HTTP请求是在 web 浏览器和服务器之间进行数据传输的重要方式。它允许用户发送请求并从服务器获取网页或其他数据。通过发送HTTP请求，我们可以浏览网页、发送电子邮件、下载文件等等。没有HTTP请求，我们无法与互联网进行交互。

## 如何发送HTTP请求
发送HTTP请求并不复杂，只需遵循一些简单的步骤即可。首先，我们需要创建一个连接到服务器的套接字。然后，我们需要使用`send()`函数向服务器发送请求。最后，我们使用`recv()`函数接收服务器返回的响应。下面是一个简单的代码示例：

```C
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>

int main() {
    int client_fd;
    struct addrinfo hints, *res;

    // 创建套接字
    client_fd = socket(AF_INET, SOCK_STREAM, 0);

    // 设定服务器信息
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    getaddrinfo("example.com", "80", &hints, &res);

    // 连接到服务器
    connect(client_fd, res->ai_addr, res->ai_addrlen);

    // 发送HTTP请求
    char *request = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
    send(client_fd, request, strlen(request), 0);

    // 接收服务器响应
    char buffer[256];
    int recv_len = recv(client_fd, buffer, 256, 0);

    // 打印响应内容
    printf("%.*s\n", recv_len, buffer);
    
    // 关闭套接字
    close(client_fd);

    return 0;
}
```

输出结果将类似于以下内容：

```
HTTP/1.1 200 OK
Date: Mon, 01 Mar 2021 12:00:00 GMT
Server: Apache/2.4.41 (Ubuntu)
Last-Modified: Sun, 28 Feb 2021 12:00:00 GMT
Content-Type: text/html
Content-Length: 13
Connection: close

Hello, world!
```

## 深入了解HTTP请求
HTTP请求通常包含一个请求行、请求头和请求体三个部分。请求行包含HTTP方法（GET、POST等）、请求的URL和HTTP协议版本。请求头包含关于请求的信息，如Host、User-Agent、Content-Type等。请求体主要用于POST请求，包含要发送的数据。除了GET和POST方法外，还有一些其他常用的HTTP方法，如PUT、DELETE、HEAD等。

除了文本内容，HTTP请求还可以包含二进制数据，如文件上传。我们可以在请求头中设置Content-Type为“multipart/form-data”，然后在请求体中添加二进制数据。服务器在接收到这样的请求后，可以通过解析请求体来获取文件。

HTTP请求也可以携带cookies和身份验证信息。通过在请求头中添加Cookie和Authorization等字段，我们可以访问需要身份验证的网页或资源。

## 参考链接
- [HTTP请求介绍](https://www.w3schools.com/whatis/whatis_http.asp)
- [HTTP请求方法](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods)
- [HTTP请求格式](https://www.runoob.com/http/http-messages.html)
- [C语言中的套接字编程](https://www.geeksforgeeks.org/socket-programming-cc/)